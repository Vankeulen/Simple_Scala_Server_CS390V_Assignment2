
import java.io._
import java.net.{ServerSocket, Socket}
import java.nio.file.{Files, Paths}
import java.util

import scala.io.BufferedSource


object EchoServer {
	def read_and_write(in: BufferedReader, out:BufferedWriter): Unit = {
		out.write(in.readLine())
		out.flush()
		in.close()
		out.close()
	}

	def serve(server: ServerSocket): Unit = {
		val s = server.accept()
		val in = new BufferedReader(new InputStreamReader(s.getInputStream))
		val out = new BufferedWriter(new OutputStreamWriter(s.getOutputStream))

		read_and_write(in, out)

		s.close()
	}

	// Default serve path
	var servePath = "."

	def main(args: Array[String]) {
		if (args.length > 0) { servePath = args(0) }
		System.out.println("user.dir = " + System.getProperty("user.dir"))

		System.out.println("Main: Serving from " + Paths.get(servePath).toAbsolutePath())

		new HttpServer(servePath).serve()
	}

}


/** Class wrapping HTTP Server behavior */
class HttpServer(val path: String) {

	var running: Boolean = true;

	def stop(): Unit = {
		running = false
	}
	def serve(): Unit = {
		val server = new ServerSocket(9999)

		while (running) {
			val socket = server.accept()

			System.out.println("Got connection from client.")
			val request = new HttpRequest(socket)

			handle(request)

			// request.finish()


			System.out.println("actually done now.")
		}
	}

	/** Simple http request handler */
	def handle(request: HttpRequest): Unit = {

		var requestPath : String = path + request.path
		if (requestPath.endsWith("/")) { requestPath = requestPath + "index.html" }

		System.out.println("Request for file at: " + Paths.get(requestPath).toAbsolutePath)

		var bytes: Array[Byte] = null
		try {
			request.print("HTTP/1.1 200 OK \r\n")
			request.print("\r\n")
			bytes = Files.readAllBytes(Paths.get(requestPath))
		} catch {
			case ex: Exception => {
				System.out.println(ex)
			}
		}

		if (bytes != null) {
			// If we have a file, respond with the file content
			request.print(bytes)
		} else {
			// Otherwise respond with a 404 response
			request.print("HTTP/1.1 404 File not found \r\n")
			request.print("\r\n")
			request.print("404 " + requestPath + " not found.")

		}
		request.finish()
	}
}



/** HttpRequest class. Takes a socket, reads the input, and provides all information about the request. */
class HttpRequest(val socket: Socket) {
	private val source : BufferedSource = new BufferedSource(socket.getInputStream())
	private val in : Iterator[String] = source.getLines()


	private val out : OutputStream = socket.getOutputStream()
	private val pout : PrintStream = new PrintStream(out)
	private val lines : List[String] = readLines(in)
	for (line <- lines) {
		System.out.println("got: " + line)
	}

	private val topLine: Array[String] = lines.head.split(" ")

	/** Http Verb, eg GET, PUT, POST, HEAD */
	val verb: String = topLine(0)
	/** Path of request */
	val path: String = topLine(1)
	/** Http version of request */
	val version: String = topLine(2)

	/** Headers included in request (we don't read these yet) */
	val headers: util.Map[String, List[String]] = new util.HashMap()

	/** Dummy finish method. Replies to request with "hi" */
	def finish(): Unit = {
		pout.flush()
		out.flush()
		socket.close()
	}

	def print(str: String): Unit = {
		pout.println(str)
	}

	def print(bytes: Array[Byte]): Unit = {
		out.write(bytes)
	}
	/** Move some nasty imperative code into its own method */
	def readLines(in: Iterator[String]): List[String] = {
		// Flag to break loop
		var go : Boolean = true;
		var lines : List[String] = List()

		while (go) {
			val line: String = in.next()

			if (line.length == 0) {
				// Empty line, stop reading.
				go = false;
			} else {
				// Append line to end of list
				lines = lines :+ line
			}
		}
		// Return value:
		lines
	}
}
