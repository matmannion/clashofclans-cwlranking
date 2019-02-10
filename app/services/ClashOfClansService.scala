package services

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.URLEncoder

import com.google.inject.ImplementedBy
import javax.inject.Inject
import play.api.Configuration
import play.api.cache.AsyncCacheApi
import play.api.http.Status
import play.api.libs.ws.WSClient
import services.ClashOfClansService._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.JsonBodyWritables._

import scala.reflect.ClassTag

object ClashOfClansService {
  case class Tag(string: String) {
    val url: String = URLEncoder.encode(string, "UTF-8")
  }
  val readsTag: Reads[Tag] = implicitly[Reads[String]].map(Tag.apply)
  val writesTag: Writes[Tag] = Writes[Tag](t => JsString(t.string))
  implicit val formatTag: Format[Tag] = Format(readsTag, writesTag)

  case class ImageUrls(
    small: Option[String],
    large: Option[String],
    medium: Option[String],
  )
  implicit val formatIconUrls: Format[ImageUrls] = Json.format[ImageUrls]

  case class League(
    id: Int,
    name: String,
    iconUrls: ImageUrls
  )
  implicit val formatLeague: Format[League] = Json.format[League]

  case class PlayerClan(
    tag: Tag,
    name: String,
    clanLevel: Int,
    badgeUrls: ImageUrls,
  )
  implicit val formatPlayerClan: Format[PlayerClan] = Json.format[PlayerClan]

  case class LegendSeasonStatistics(
    rank: Int,
    trophies: Int,
    id: String,
  )
  implicit val formatLegendSeasonStatistics: Format[LegendSeasonStatistics] = Json.format[LegendSeasonStatistics]

  case class LegendStatistics(
    legendTrophies: Int,
    currentSeason: LegendSeasonStatistics,
    previousSeason: LegendSeasonStatistics,
    bestSeason: LegendSeasonStatistics,
  )
  implicit val formatLegendStatistics: Format[LegendStatistics] = Json.format[LegendStatistics]

  case class Achievement(
    name: String,
    stars: Int,
    value: Int,
    target: Int,
    info: String,
    village: String,
  )
  implicit val formatAchievement: Format[Achievement] = Json.format[Achievement]

  case class TroopHeroSpell(
    name: String,
    level: Int,
    maxLevel: Int,
    village: String,
  ) {
    val offensiveWeight: Int = name match {
      case "Barbarian King" => level * 20
      case "Archer Queen" => level * 30
      case "Grand Warden" => level * 40

      case "Lightning Spell" => level * 200
      case "Healing Spell" => level * 210
      case "Rage Spell" => level * 220
      case "Jump Spell" => level * 230
      case "Freeze Spell" => level * 240
      case "Clone Spell" => level * 250
      case "Poison Spell" => level * 125
      case "Earthquake Spell" => level * 100
      case "Haste Spell" => level * 150
      case "Skeleton Spell" => level * 175
      case "Bat Spell" => level * 200

      case "Barbarian" => level * 50
      case "Archer" => level * 75
      case "Goblin" => level * 100
      case "Giant" => level * 125
      case "Wall Breaker" => level * 140
      case "Balloon" => level * 120
      case "Wizard" => level * 105
      case "Healer" => level * 140
      case "Dragon" => level * 150
      case "P.E.K.K.A" => level * 150
      case "Baby Dragon" => level * 155
      case "Miner" => level * 160
      case "Electro Dragon" => level * 165

      case "Minion" => level * 120
      case "Hog Rider" => level * 150
      case "Valkyrie" => level * 170
      case "Golem" => level * 333
      case "Witch" => level * 800
      case "Lava Hound" => level * 625
      case "Bowler" => level * 1600
      case "Ice Golem" => level * 1600

      case "Wall Wrecker" => level * 300
      case "Battle Blimp" => level * 300
      case "Stone Slammer" => level * 300

      case _ if village != "home" => 0
    }
  }
  implicit val formatTroopHeroSpell: Format[TroopHeroSpell] = Json.format[TroopHeroSpell]

  case class Player(
    tag: Tag,
    name: String,
    expLevel: Int,
    league: Option[League],
    trophies: Int,
    versusTrophies: Int,
    attackWins: Int,
    defenseWins: Int,
    clan: Option[PlayerClan],
    bestTrophies: Int,
    donations: Int,
    donationsReceived: Int,
    warStars: Int,
    role: Option[String],
    townHallLevel: Int,
    builderHallLevel: Int,
    bestVersusTrophies: Int,
    versusBattleWins: Int,
//    legendStatistics: LegendStatistics,
    achievements: Seq[Achievement],
    troops: Seq[TroopHeroSpell],
    heroes: Seq[TroopHeroSpell],
    spells: Seq[TroopHeroSpell],
  ) {
    val offensiveWeight = troops.map(_.offensiveWeight).sum + heroes.map(_.offensiveWeight).sum + spells.map(_.offensiveWeight).sum
  }
  implicit val formatPlayer: Format[Player] = Json.format[Player]

  case class ClanPlayer(
    tag: Tag,
    name: String,
    expLevel: Int,
    league: Option[League],
    trophies: Int,
    versusTrophies: Int,
    role: Option[String],
    clanRank: Int,
    previousClanRank: Int,
    donations: Int,
    donationsReceived: Int,
  )
  implicit val formatClanPlayer: Format[ClanPlayer] = Json.format[ClanPlayer]

  case class Clan(
    tag: Tag,
    name: String,
    badgeUrls: ImageUrls,
    clanLevel: Int,
    clanPoints: Int,
    clanVersusPoints: Int,
    members: Int,
    `type`: String,
    requiredTrophies: Int,
    warFrequency: String,
    warWinStreak: Int,
    warWins: Int,
    warTies: Option[Int], // Only if public
    warLosses: Option[Int], // Only if public
    isWarLogPublic: Boolean,
    description: String,
    memberList: Seq[ClanPlayer],
  )
  implicit val formatClan: Format[Clan] = Json.format[Clan]

  case class ClanWarAttack(
    attackerTag: Tag,
    defenderTag: Tag,
    stars: Int,
    destructionPercentage: Int,
    order: Int,
  )
  implicit val formatClanWarAttack: Format[ClanWarAttack] = Json.format[ClanWarAttack]

  case class ClanWarMember(
    tag: Tag,
    name: String,
    townhallLevel: Int,
    mapPosition: Int,
    attacks: Option[Seq[ClanWarAttack]],
    opponentAttacks: Int,
    bestOpponentAttack: Option[ClanWarAttack],
  )
  implicit val formatClanWarMember: Format[ClanWarMember] = Json.format[ClanWarMember]

  case class ClanWarClan(
    tag: Tag,
    name: String,
    badgeUrls: ImageUrls,
    clanLevel: Int,
    attacks: Int,
    stars: Int,
    expEarned: Option[Int],
    members: Seq[ClanWarMember],
  )
  implicit val formatClanWarClan: Format[ClanWarClan] = Json.format[ClanWarClan]

  case class ClanWar(
    state: String,
    teamSize: Int,
    preparationStartTime: String,
    startTime: String,
    endTime: String,
    clan: ClanWarClan,
    opponent: ClanWarClan,
  )
  implicit val formatClanWar: Format[ClanWar] = Json.format[ClanWar]

  case class ClanWarLeagueClanMember(
    tag: Tag,
    name: String,
    townHallLevel: Int,
  )
  implicit val formatClanWarLeagueClanMember: Format[ClanWarLeagueClanMember] = Json.format[ClanWarLeagueClanMember]

  case class ClanWarLeagueClan(
    tag: Tag,
    name: String,
    clanLevel: Int,
    badgeUrls: ImageUrls,
    members: Seq[ClanWarLeagueClanMember],
  )
  implicit val formatClanWarLeagueClan: Format[ClanWarLeagueClan] = Json.format[ClanWarLeagueClan]

  case class ClanWarLeagueRound(
    warTags: Seq[Tag],
  )
  implicit val formatClanWarLeagueRound: Format[ClanWarLeagueRound] = Json.format[ClanWarLeagueRound]

  case class ClanWarLeague(
    state: String,
    season: String,
    clans: Seq[ClanWarLeagueClan],
    rounds: Seq[ClanWarLeagueRound],
  )
  implicit val formatClanWarLeague: Format[ClanWarLeague] = Json.format[ClanWarLeague]
}

@ImplementedBy(classOf[ClashOfClansServiceImpl])
trait ClashOfClansService {
  def clan(tag: Tag): Future[Either[String, Clan]]
  def currentWar(tag: Tag): Future[Either[String, ClanWar]]
  def currentWarLeague(tag: Tag): Future[Either[String, ClanWarLeague]]
  def clanWarLeagueWar(tag: Tag): Future[Either[String, ClanWar]]
  def player(tag: Tag): Future[Either[String, Player]]
}

class ClashOfClansServiceImpl @Inject()(
  ws: WSClient,
  cache: AsyncCacheApi,
  configuration: Configuration,
)(implicit executionContext: ExecutionContext) extends ClashOfClansService {

  private[this] val baseUri = configuration.get[String]("clashofclans.baseUri")
  private[this] val apiKey = configuration.get[String]("clashofclans.apiKey")

  private def cached[A: ClassTag](key: String, expiration: Duration)(fn: => Future[Either[String, A]])(implicit fmt: Format[A]): Future[Either[String, A]] = {
    val json = new File(s"/tmp/$key")
    if (json.exists()) {
      val is = new FileInputStream(json)
      try {
        Json.parse(is).validate[A].fold(
          errors => Future.successful(Left(errors.map(_.toString()).mkString(", "))),
          json => Future.successful(Right(json))
        )
      } finally {
        is.close()
      }
    } else {
      cache.get[Either[String, A]](key).flatMap {
        case Some(value) => Future.successful(value)
        case _ => fn.flatMap(_.fold(
          errors => Future.successful(Left(errors)), // Do not cache errors
          result => cache.set(key, result, expiration).map { _ =>
            json.getParentFile.mkdirs()
            val os = new FileOutputStream(json)
            os.write(Json.toBytes(Json.toJson(result)))
            os.close()
            Right(result)
          }
        ))
      }
    }
  }

  private def apiGet[A](endpoint: String)(implicit rds: Reads[A]): Future[Either[String, A]] =
    ws.url(s"$baseUri$endpoint")
      .withHttpHeaders(
        "Authorization" -> s"Bearer $apiKey",
        "Accept" -> "application/json",
      )
      .get()
      .map { response =>
        if (response.status != Status.OK) Left(s"Invalid response code for $endpoint: ${response.status}. ${response.body}")
        else response.json.validate[A].fold(
          errors => Left(errors.map(_.toString()).mkString(", ")),
          Right.apply
        )
      }

  def clan(tag: Tag): Future[Either[String, Clan]] =
    cached[Clan](s"clan:${tag.string}", 1.hour) {
      apiGet[Clan](s"/clans/${tag.url}")
    }

  def currentWar(tag: Tag): Future[Either[String, ClanWar]] =
    cached[ClanWar](s"clan:currentwar:${tag.string}", 10.minutes) {
      apiGet[ClanWar](s"/clans/${tag.url}/currentwar")
    }

  def currentWarLeague(tag: Tag): Future[Either[String, ClanWarLeague]] =
    cached[ClanWarLeague](s"clan:currentwarleague:${tag.string}", 1.day) {
      apiGet[ClanWarLeague](s"/clans/${tag.url}/currentwar/leaguegroup")
    }

  def clanWarLeagueWar(tag: Tag): Future[Either[String, ClanWar]] =
    cached[ClanWar](s"clanwarleaguewar:${tag.string}", 10.minutes) {
      apiGet[ClanWar](s"/clanwarleagues/wars/${tag.url}")
    }

  def player(tag: Tag): Future[Either[String, Player]] =
    cached[Player](s"player:${tag.string}", 1.day) {
      apiGet[Player](s"/players/${tag.url}")
    }

}
