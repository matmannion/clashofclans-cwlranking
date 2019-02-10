package controllers

import controllers.HomeController._
import javax.inject._
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import services.ClashOfClansService
import services.ClashOfClansService._

import scala.concurrent.{ExecutionContext, Future}

object HomeController {
  case class RichClanWarMember(
    player: Player,
    position: Int,
    mapPosition: Int,
  )
  implicit val writesRichClanWarMember: Writes[RichClanWarMember] = Json.writes[RichClanWarMember]

  case class RichClanWarAttack(
    attacker: RichClanWarMember,
    defender: RichClanWarMember,
    stars: Int,
    gainedStars: Int,
    destruction: Int,
  ) {
    // Use position, not mapPosition (just trying to match fly)
    val clashCaller: Double = 1.25 * (2 - defender.position.toDouble / 15) * gainedStars + (1.0 / 36) * gainedStars
  }
  implicit val writesRichClanWarAttack: Writes[RichClanWarAttack] = Json.writes[RichClanWarAttack]

  case class RichClanWarMemberWithAttacks(
    player: Player,
    position: Int,
    mapPosition: Int,
    attack: Option[RichClanWarAttack],
    bestOpponentAttack: Option[RichClanWarAttack],
  )
  implicit val writesRichClanWarMemberWithAttacks: Writes[RichClanWarMemberWithAttacks] = Json.writes[RichClanWarMemberWithAttacks]

  case class RichClanWarClan(
    tag: Tag,
    name: String,
    attacks: Int,
    stars: Int,
    members: Seq[RichClanWarMemberWithAttacks],
  )
  implicit val writesRichClanWarClan: Writes[RichClanWarClan] = Json.writes[RichClanWarClan]

  case class RichClanWar(
    clan: RichClanWarClan,
    opponent: RichClanWarClan,
  )
  implicit val writesRichClanWar: Writes[RichClanWar] = Json.writes[RichClanWar]

  object RichClanWar {
    def apply(war: ClanWar, players: Map[Tag, Player]): RichClanWar = {
      def baseMembers(clan: ClanWarClan): Map[Tag, RichClanWarMember] =
        clan.members.zipWithIndex.map { case (member, position) =>
          member.tag -> RichClanWarMember(
            player = players(member.tag),
            position = position + 1,
            mapPosition = member.mapPosition,
          )
        }.toMap

      val allBaseMembers: Map[Tag, RichClanWarMember] = baseMembers(war.clan) ++ baseMembers(war.opponent)

      def richClanWarClan(clan: ClanWarClan): RichClanWarClan = {
        def richClanWarAttack(attack: ClanWarAttack, isAttack: Boolean): RichClanWarAttack = {
          val previousAttacks: Seq[ClanWarAttack] =
            if (isAttack)
              clan.members.flatMap(_.attacks.getOrElse(Nil))
                .filter { previous => attack.order > previous.order && attack.defenderTag == previous.defenderTag }
            else
              clan.members.flatMap(_.bestOpponentAttack)
                .filter { previous => attack.order > previous.order && attack.defenderTag == previous.defenderTag }

          val gainedStars =
            if (previousAttacks.isEmpty) attack.stars
            else Math.max(0, attack.stars - previousAttacks.map(_.stars).max)

          RichClanWarAttack(
            attacker = allBaseMembers(attack.attackerTag),
            defender = allBaseMembers(attack.defenderTag),
            stars = attack.stars,
            gainedStars = gainedStars,
            destruction = attack.destructionPercentage,
          )
        }

        val members: Seq[RichClanWarMemberWithAttacks] = clan.members.map { member =>
          RichClanWarMemberWithAttacks(
            player = allBaseMembers(member.tag).player,
            position = allBaseMembers(member.tag).position,
            mapPosition = allBaseMembers(member.tag).mapPosition,
            attack = member.attacks.flatMap(_.headOption).map(richClanWarAttack(_, isAttack = true)),
            bestOpponentAttack = member.bestOpponentAttack.map(richClanWarAttack(_, isAttack = false)),
          )
        }

        RichClanWarClan(
          tag = clan.tag,
          name = clan.name,
          attacks = clan.attacks,
          stars = clan.stars,
          members = members,
        )
      }

      RichClanWar(
        clan = richClanWarClan(war.clan),
        opponent = richClanWarClan(war.opponent),
      )
    }
  }

  case class PlayerStatsAttack(
    position: Int,
    opponentPosition: Int,
    stars: Int,
    gainedStars: Int,
    clashCaller: Double,
    destruction: Int,
  )
  implicit val writesPlayerStatsAttack: Writes[PlayerStatsAttack] = Json.writes[PlayerStatsAttack]

  case class PlayerStats(
    tag: Tag,
    name: String,
    stars: Int,
    netStars: Int,
    clashCaller: Double,
    netClashCaller: Double,
    destruction: Int,
    attacks: Seq[PlayerStatsAttack],
    defences: Seq[PlayerStatsAttack],
  )
  implicit val writesPlayerStats: Writes[PlayerStats] = Json.writes[PlayerStats]
}

@Singleton
class HomeController @Inject()(
  cc: ControllerComponents,
  clashOfClans: ClashOfClansService,
)(implicit executionContext: ExecutionContext) extends AbstractController(cc) with ControllerImplicits {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index(): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    clashOfClans.clan(Tag("#90JJLUCQ")).successFlatMap { clan =>
      // Get the current war league
      clashOfClans.currentWarLeague(clan.tag).successFlatMap { warLeague =>
        // Get all the wars that the clan was involved in
        Future.sequence(warLeague.rounds.flatMap(_.warTags).map(clashOfClans.clanWarLeagueWar)).flat.successFlatMap { wars =>
          val relevantWars = wars
              .filter { war => war.clan.tag == clan.tag || war.opponent.tag == clan.tag }
              .map { war =>
                if (war.clan.tag == clan.tag) war
                else war.copy(clan = war.opponent, opponent = war.clan) // Swap so our clan is always first
              }
              .map { war =>
                // Only include members with an attack or an opposition attack
                def filterMembers(clan: ClanWarClan): ClanWarClan =
                  clan.copy(members = clan.members.filter { member => member.attacks.nonEmpty || member.opponentAttacks > 0 }.sortBy(_.mapPosition))

                war.copy(clan = filterMembers(war.clan), opponent = filterMembers(war.opponent))
              }

          // Get all player tags to fetch
          val playerTags =
            relevantWars.flatMap { war => war.clan.members.map(_.tag) ++ war.opponent.members.map(_.tag) }
              .distinct

          Future.sequence(playerTags.map(clashOfClans.player)).flat.successMap { players =>
            val playersMap = players.map { p => p.tag -> p }.toMap
            val wars = relevantWars.map(RichClanWar(_, playersMap))

            // Collect player stats
            Ok(views.html.index(wars.flatMap(_.clan.members.map(_.player)).distinct.map { player =>
              val attacks = wars.flatMap(_.clan.members.filter(_.player.tag == player.tag).flatMap(_.attack))
              val defences = wars.flatMap(_.clan.members.filter(_.player.tag == player.tag).flatMap(_.bestOpponentAttack))

              PlayerStats(
                tag = player.tag,
                name = player.name,
                stars = attacks.map(_.gainedStars).sum,
                netStars = attacks.map(_.gainedStars).sum - defences.map(_.stars).sum,
                clashCaller = attacks.map(_.clashCaller).sum,
                netClashCaller = attacks.map(_.clashCaller).sum - defences.map(_.clashCaller).sum,
                destruction = attacks.map(_.destruction).sum,
                attacks = attacks.map { attack =>
                  PlayerStatsAttack(
                    position = attack.attacker.position,
                    opponentPosition = attack.defender.position,
                    stars = attack.stars,
                    gainedStars = attack.gainedStars,
                    clashCaller = attack.clashCaller,
                    destruction = attack.destruction,
                  )
                },
                defences = defences.map { attack =>
                  PlayerStatsAttack(
                    position = attack.attacker.position,
                    opponentPosition = attack.defender.position,
                    stars = attack.stars,
                    gainedStars = attack.gainedStars,
                    clashCaller = attack.clashCaller,
                    destruction = attack.destruction,
                  )
                }
              )
            }))
          }
        }
      }
    }
  }
}
