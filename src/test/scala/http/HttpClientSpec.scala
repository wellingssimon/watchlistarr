package http

import cats.effect.{IO, Resource}
import cats.syntax.all._
import com.comcast.ip4s._
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Server
// import org.scalatest.wordspec.AsyncWordSpec // No longer needed
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers // Ensure Matchers is imported and used

class HttpClientSpec extends AsyncIOSpec with Matchers {

  // Define the service for the test server
  val testService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "test" / id =>
      Ok(Json.obj("id" -> Json.fromString(id), "message" -> Json.fromString("Ok")))
  }

  // Resource for the test server
  val serverResource: Resource[IO, Server] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"0") // Use port 0 to let the system pick an available port
    .withHttpApp(testService.orNotFound)
    .build

  // Resource for the http4s client
  val clientResource: Resource[IO, Client[IO]] = EmberClientBuilder
    .default[IO]
    .build

  "HttpClient" should {
    "stress test HttpClient with multiple requests" in {
      val numRequests = 500
      val uniqueRequests = numRequests / 2

      (serverResource, clientResource).tupled.use { case (server, liveHttpClient) =>
        val httpClient = new HttpClient(liveHttpClient) // Use the live client from the resource

        val baseUri = s"http://${server.address.getHostString}:${server.address.getPort}/test"

        // Generate 250 unique URIs and 250 duplicates
        val uris = (0 until uniqueRequests).map(i => Uri.unsafeFromString(s"$baseUri/$i")).toList ++
                   (0 until uniqueRequests).map(i => Uri.unsafeFromString(s"$baseUri/$i")).toList

        uris.length shouldBe numRequests // Quick check

        val resultsIO: IO[List[Either[Throwable, Json]]] =
          Traverse[List].traverse(uris)(uri => httpClient.httpRequest(Method.GET, uri))

        resultsIO.asserting { results =>
          results.foreach { result =>
            result shouldBe a[Right[_, _]] // Uses Matchers
            result.foreach { json =>
              json.hcursor.downField("message").as[String] shouldBe Right("Ok") // Uses Matchers
            }
          }
          results.length shouldBe numRequests // Uses Matchers
          results.count(_.isRight) shouldBe numRequests // Uses Matchers
        }
      }
    }
  }
}
