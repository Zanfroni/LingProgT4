(* REFERENCIAS *)
(* https://learnxinyminutes.com/docs/standard-ml/ *)
(* http://sml-family.org/Basis/string.html *)
(* https://www.freeformatter.com/string-utilities.html *)
(* https://www.cs.cmu.edu/afs/cs/academic/class/15814-f03/www/sml-intro.pdf *)
(* https://www.cs.cmu.edu/~rwh/introsml/core/datatypes.htm *)
(* https://stackoverflow.com/questions/43729134/how-to-output-a-string-list-to-a-textfile-in-sml *)

(* As duas funcoes seguintes filtra palavras erradas usadas no portugues *)
fun purge(noAcc) = 
  	let val noAccents = explode "ãáàâçéêóô!?*+-#$%()&0123456789"
  	in List.all(Char.notContains noAcc) noAccents
  	end

fun clearAccents(newlist) = List.filter purge newlist

(* Esta funcao reduz os caracteres para lowercase *)
val lowercase = String.map Char.toLower;


(* Esta funcao le arquivos dos fragmentos e palavras *)
(* Esta funcao foi adaptada de https://learnxinyminutes.com/docs/standard-ml/ *)
fun readFile(filename) = 
	let val file = TextIO.openIn filename
	    val words = TextIO.inputAll file
	    val _ = TextIO.closeIn file
	    val toFilter = String.tokens(fn c => c = #"\n") words
	in clearAccents(toFilter)
	end
	
(* Esta funcao deveria ser o gerador do txt validado *)
(* Esta funcao foi adaptada de https://stackoverflow.com/questions/43729134/how-to-output-a-string-list-to-a-textfile-in-sml *)
(* Ela deve pegar a lista de palavras validadas e escrever em txt *)
(* Cada palavra da lista e separada por \n *)
fun writeFile(list) =
  let val outStream = TextIO.openOut "feito.txt"
      fun out(xs) =  
          case xs of
              [] => (TextIO.closeOut outStream)
              | x::xs' => (TextIO.output(outStream, x ^ "\r\n"); out(xs'))
  in out(list)
  end;

(* Variaveis que sao os arquivos de entrada a serem lidos, ja filtrados e em lowercase *)
val file_frag = List.map lowercase(readFile("fragments.txt"));
val file_words = List.map lowercase(readFile("words.txt"));

writeFile(["oi", "tchau", "teste"]);

(* Nao conseguiu-se implementar o validador *)
(* Nao viu-se necessario o uso de DataTypes *)
(* O que voce quis dizer com otimizacao por paralelismo *)
(* Voce nao deixou claro sobre testes automatizados em Java *)
