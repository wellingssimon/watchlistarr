import cats.effect._
import cats.implicits.catsSyntaxTuple4Parallel
import configuration.{Configuration, ConfigurationUtils, FileAndSystemPropertyReader}
import http.HttpClient
import org.http4s.client.Client
import org.http4s.client.middleware.FollowRedirect
import org.http4s.ember.client.EmberClientBuilder
import org.slf4j.LoggerFactory

import java.nio.channels.ClosedChannelException
import scala.concurrent.duration.DurationInt

object Server extends IOApp {

  private val logger = LoggerFactory.getLogger(getClass)

  override protected def reportFailure(err: Throwable): IO[Unit] = err match {
    case _: ClosedChannelException => IO.pure(logger.debug("Suppressing ClosedChannelException error", err))
    case _                         => IO.pure(logger.error("Failure caught and handled by IOApp", err))
  }

  def run(args: List[String]): IO[ExitCode] = {
    val configReader = FileAndSystemPropertyReader
    val httpClientResource = EmberClientBuilder.default[IO].build.map(FollowRedirect(5))

    httpClientResource.use { implicit resolvedHttpClient: Client[IO] =>

      val appHttpClient = new http.HttpClient(resolvedHttpClient)
      for {
        initialConfig <- ConfigurationUtils.create(configReader, appHttpClient)
        configRef     <- Ref.of[IO, Configuration](initialConfig)
        result <- (
          pingTokenSync(configRef, appHttpClient),
          plexRssSync(configRef, appHttpClient),
          plexTokenDeleteSync(configRef, appHttpClient),
          plexFullSync(configRef, appHttpClient)
        ).parTupled.as(ExitCode.Success)
      } yield result
    }
  }

  private def fetchLatestConfig(configRef: Ref[IO, Configuration]): IO[Configuration] =
    configRef.get

  private def pingTokenSync(configRef: Ref[IO, Configuration], appHttpClient: HttpClient): IO[Unit] =
    for {
      config <- fetchLatestConfig(configRef)
      _      <- PingTokenSync.run(config, appHttpClient)
      _      <- IO.sleep(24.hours)
      _      <- pingTokenSync(configRef, appHttpClient)
    } yield ()

  private def plexRssSync(
      configRef: Ref[IO, Configuration],
      appHttpClient: HttpClient
  ): IO[Unit] =
    for {
      config <- fetchLatestConfig(configRef)
      _      <- PlexTokenSync.run(config, appHttpClient, runFullSync = false)
      _      <- IO.sleep(config.refreshInterval)
      _      <- plexRssSync(configRef, appHttpClient)
    } yield ()

  private def plexFullSync(
      configRef: Ref[IO, Configuration],
      appHttpClient: HttpClient
  ): IO[Unit] =
    for {
      config <- fetchLatestConfig(configRef)
      _      <- PlexTokenSync.run(config, appHttpClient, runFullSync = true)
      _      <- IO.sleep(19.minutes)
      _      <- plexFullSync(configRef, appHttpClient)
    } yield ()

  private def plexTokenDeleteSync(configRef: Ref[IO, Configuration], appHttpClient: HttpClient): IO[Unit] =
    for {
      config <- fetchLatestConfig(configRef)
      _      <- PlexTokenDeleteSync.run(config, appHttpClient)
      _      <- IO.sleep(config.deleteConfiguration.deleteInterval)
      _      <- plexTokenDeleteSync(configRef, appHttpClient)
    } yield ()
}
