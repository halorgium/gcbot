structure GCBot = 
struct
  val server = "irc.undernet.org"
  val port = 6667
  val nick = "gcbot"
  val realname = "G. C. Bot"
  val chan = "#geekcentral"

  exception ParseError
  
  val addr = valOf (NetHostDB.getByName server)

  val sock : Socket.active INetSock.stream_sock = INetSock.TCP.socket()
  
  fun send(sock,str) = 
    Socket.sendVec(sock,Word8VectorSlice.full(Word8Vector.fromList(map Word8.fromInt (map Char.ord (String.explode(str))))))
  fun recvn(sock,size) = 
    String.implode (map Char.chr (map Word8.toInt (Word8Vector.foldr (fn (x,y) => x::y) [] (Socket.recvVec(sock,size)))))
  fun recv(sock) = recvn(sock, String.maxSize)

  fun main (stateFile) =
  let
	val _ = (case stateFile of NONE => Interpreter.Nil
                   | SOME x => 
		let
		   val f = TextIO.openIn x
                   fun readIn () = 
	             let 
			val l = TextIO.inputLine f 
		     in 
			(if l = NONE then Interpreter.Nil else 
				(Interpreter.interpret (Parse.parse (valOf l), valOf l));readIn()) handle (Fail z) => Interpreter.Nil
		     end handle Io => Interpreter.Nil
		in
		    readIn ()
	        end)

	fun parseAndSend s = let
		val p =  Interpreter.interpret ((Parse.parse s) handle (e1) => raise (Fail "Parse error"), s) handle (Fail e2) => (Interpreter.Print("I'm confused: " ^ e2))
	in
		case p of Interpreter.Nil => ()
                   | Interpreter.Raw rw => ignore(send(sock,rw ^ "\n"))
                   | x => ignore (send(sock,"PRIVMSG " ^ chan ^ " :" ^ Interpreter.printable(x) ^ "\n"))
	end

	fun pingPong s = 
	  let 
	     val _ = print (s ^ "\n") 
	     val _ = if (String.isPrefix "PING " s) then (print ("PONG " ^ (String.extract (s, 6, NONE)) ^ "\n"); 
	                ignore (send(sock,"PONG " ^ (String.extract (s, 5, NONE))))) else ()

	(* NOTICE AUTH :*** Your ident is disabled or broken, to continue to connect you must type /QUOTE PASS 83251 *)
	     val _ = if (String.isSubstring "QUOTE PASS" s) then let val p = String.tokens (fn x => x = #" ") s
		     in ignore(send(sock, "PASS " ^ (List.nth (p,16)) ^ "\n")) end else ()
	in
		()
	end

	fun getNick s = List.nth(String.fields (fn x => x = #"!") s,0)

	fun chanHandle s = 
	  let
	    (* :gian_!~gdp3@howrah.cs.waikato.ac.nz PRIVMSG #geekcentral :gcbot: Success! *)
	      val fields = String.fields (fn x => x = #":") s

	      val _ = if (String.isSubstring ("PRIVMSG " ^ chan) (List.nth(fields,1))) then
		(if length fields >= 3 andalso List.nth(fields,2) = nick then parseAndSend(String.concatWith ":" (List.drop(fields,3))) else ())
		else ()

	      val _ = if (String.isSubstring ("JOIN " ^ chan) s) then parseAndSend("_on_join \""^getNick(List.nth(fields,1))^"\"") else ()
	  in
		()
	  end

	fun connectLoop () = 
	let
		val i = recv(sock)
		val _ = pingPong i
		val _ = chanHandle i
	in
		if ((i = "") orelse (i = "\n")) then (print "Exiting connect loop\n") else connectLoop ()
	end

  	val _ = Socket.connect (sock, INetSock.toAddr(NetHostDB.addr addr, port))

	val _ = pingPong (recv(sock))
	val _ = pingPong (recv(sock))
	val _ = pingPong (recv(sock))

	val _ = send(sock, "NICK " ^ nick ^ "\n")
	
	val _ = pingPong (recv(sock))

	val _ = send(sock, "USER gcbot howrah irc :" ^ realname ^ "\n")

	val _ = pingPong (recv(sock))

	val _ = send(sock, "JOIN " ^ chan ^ "\n")

	val _ = connectLoop ()

	
  in
    ()
  end
end
  
