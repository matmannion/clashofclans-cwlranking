package controllers

import play.api.mvc.{RequestHeader, Result, Results}

import scala.concurrent.{ExecutionContext, Future}

trait ControllerImplicits extends Results {
  def showErrors(errors: String): Result = BadRequest(errors)

  implicit class FutureEitherControllerOps[A](val future: Future[Either[String, A]]) {
    def successMap(fn: A => Result)(implicit r: RequestHeader, ec: ExecutionContext): Future[Result] =
      future.map { result =>
        result.fold(showErrors, fn)
      }

    def successFlatMap(fn: A => Future[Result])(implicit r: RequestHeader, ec: ExecutionContext): Future[Result] =
      future.flatMap { result =>
        result.fold(
          e => Future.successful(showErrors(e)),
          fn
        )
      }
  }

  implicit class FutureSeqEitherControllerOps[A](val future: Future[Seq[Either[String, A]]]) {
    def flat(implicit ec: ExecutionContext): Future[Either[String, Seq[A]]] =
      future.map(_.partition(_.isLeft) match {
        case (Nil, results) => Right(results.map(_.right.get))
        case (errors, _) => Left(errors.map(_.left.get).mkString(", "))
      })
  }
}
