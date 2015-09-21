object foo {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("Welcome to the Scala worksheet");$skip(31); 
  val o: Option[String] = None;System.out.println("""o  : Option[String] = """ + $show(o ));$skip(33); 
  def barf = throw new Exception;System.out.println("""barf: => Nothing""");$skip(117); 
  try {
    val e = o.getOrElse(barf)
    println("E is " + e)
  } catch {
    case ex: Exception => println(ex)
  };$skip(75); 
                                                  
  println("Got here!!")}
  //e
}
