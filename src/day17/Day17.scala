package day17

/**
 * You're trying to access a secure vault protected by a 4x4 grid of small rooms connected by doors. 
 * You start in the top-left room (marked S), and you can access the vault (marked V) once you reach 
 * the bottom-right room:

   #########
   #S| | | #
   #-#-#-#-#
   # | | | #
   #-#-#-#-#
   # | | | #
   #-#-#-#-#
   # | | |  
   ####### V

 * Fixed walls are marked with #, and doors are marked with - or |.

 * The doors in your current room are either open or closed (and locked) based on the hexadecimal 
 * MD5 hash of a passcode (your puzzle input) followed by a sequence of uppercase characters 
 * representing the path you have taken so far (U for up, D for down, L for left, and R for right).

 * Only the first four characters of the hash are used; 
 * they represent, respectively, the doors up, down, left, and right from your current position. 
 * Any b, c, d, e, or f means that the corresponding door is open; 
 * any other character (any number or a) means that the corresponding door is closed and locked.

 * Given your vault's passcode, what is the shortest path (the actual path, not just the length)
 * to reach the vault?
 */
object Day17{

  def main(args: Array[String]): Unit = {
    val input = "dmypynyp"
  }

}
