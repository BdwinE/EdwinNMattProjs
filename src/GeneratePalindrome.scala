import scala.collection.mutable.ArrayBuffer

class GeneratePalindrome {
  var n:Int =14
  var m:Int = _
  var palindromes:ArrayBuffer[String] = new ArrayBuffer[String]//list of palindromes
  var hashTable: HashTable = new HashTable(n)

  def buildOut: Unit ={//builds
    //palindromes.append(n+"")//add parent palindrome
    hashTable.add(n+"")
    var pal = ""
    for(i <- 0 to n-1){
      pal += " 1"
    }
    //palindromes.append(pal.substring(1))
    hashTable.add(pal.substring(1))
    buildIn(pal.substring(1))

    /*palindromes.append(n+"")//add parent palindrome
    buildOutHelper(n, 2)*/
  }
  def buildOutHelper(currentN:Int, level:Int): Unit ={//level=  the level we are currently on
    if(currentN==1) {//base case, even though it might still make a recursive call
      if(level<=palindromes.length && palindromes(level-1).length>0) {
        palindromes(level-1) += " 1"
        buildOutHelper(1, level+1)
      }
      return
    }
    var nHalf = currentN/2
    if(currentN%2==0){//if n is divisible by 2
      if(level>palindromes.length) {
        palindromes.append(nHalf.toInt +" " + nHalf.toInt)//start new level
      } else {
        palindromes(level-1) += " " + nHalf.toInt + " " + nHalf.toInt
      }
      buildOutHelper(nHalf.toInt, level+1)//build next level
      buildOutHelper(nHalf.toInt, level+1)//build next level
    }
    else{
      if(level>palindromes.length) {
        palindromes.append(nHalf.toInt+" 1 "+ nHalf.toInt)//start new level
      } else {
        palindromes(level-1) += " " + nHalf.toInt + " 1 " + nHalf.toInt
      }
      buildOutHelper(nHalf.toInt, level+1)//build next level
      buildOutHelper(1, level+1)//build next
      buildOutHelper(nHalf.toInt, level+1)//build next
    }
  }

  def buildIn(pal:String): Unit ={
    val lastPal = getArrayBuffer(pal.split(" "))
    //var lastPal = getArrayBuffer(palindromes(palindromes.length-1).split(" "))//get last palindrome in list of palindromes
    buildInHelper(lastPal, 1)
  }
  def buildInHelper(pal:ArrayBuffer[String], level:Int): Unit ={
    //var pal = getArrayBuffer(palindrome.split(" ").array)//get all nums in the palindrome (in an array)
    if(pal.length<=1)//base case
      return
    if(level!=1) {
      hashTable.add(pal.toString().substring(12,pal.toString().length-1).replaceAll(",",""))
      /*
      var inPals = false//will be true if value in palindromes
      for(i <- 0 to palindromes.length-1){
        if(palindromes(i).equals(pal.toString().substring(12,pal.toString().length-1).replaceAll(",","")))//know this is lazy coding sorry, all I did was remove "ArrayBuffer" and ")" from the string
          inPals = true
      }
      if(!inPals)
        palindromes.append(pal.toString().substring(12,pal.toString().length-1).replaceAll(",",""))//append to list if this palindrome isn't already in it*/
    }
    var palCopy = copy(pal)
    //start parsing tree
    if(pal.length>3){
      if(pal.length%2==0){//the length of the panlindrome is even
        var midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        palCopy.remove(midPoint)//handle middle two nums first
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint).trim.toInt)
        buildInHelper(palCopy, level+1)

        //restore palCopy to orginal


        //do it for rest of pal TODO test
        var left2Pal = ""
        var right2Pal = ""
        for(i <- 1 to pal.length/2-1){
          palCopy = pal.clone()//copy(pal)
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(pal.length-i).trim.toInt  + palCopy(pal.length-1-i).trim.toInt)
          palCopy.remove(i-1); palCopy(i-1) = left2Pal
          palCopy.remove(palCopy.length - i); palCopy(palCopy.length - i) = right2Pal
          //println("parent: " + pal + " child: " + palCopy)
          buildInHelper(palCopy, level+1)
        }
      }
      else{//pal is odd
        var midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        palCopy.remove(midPoint) //-= palCopy(midPoint)//handle middle three nums first
        midVal = "" + (midVal.trim.toInt + palCopy(midPoint).trim.toInt)
        palCopy.remove(midPoint) //-= palCopy(midPoint)
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint).trim.toInt)
        //println("parent: " + pal + " child: " + palCopy)
        buildInHelper(palCopy, level+1)

        //do it for rest of pal
        var left2Pal = ""
        var right2Pal = ""
        for(i <- 1 to pal.length/2-1){
          palCopy = copy(pal)
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(pal.length-i).trim.toInt + palCopy(pal.length-1-i).trim.toInt)
          palCopy.remove(i-1); palCopy(i-1) = left2Pal
          palCopy.remove(palCopy.length-i); palCopy(palCopy.length-i)=right2Pal
          //println("parent: " + pal + " child: " + palCopy)
          buildInHelper(palCopy, level+1)
        }
      }
    }
    else{//pal.length is less than 3
      if(pal.length == 3){
        //print("parent: " + pal)
        pal(0) = " " + (pal(0).trim.toInt + pal(1).trim.toInt + pal(2).trim.toInt)
        pal.remove(1)
        pal.remove(1)
        //println(" child: " + pal)
        buildInHelper(pal, level+1)
      }
      else{//pal.length == 2
        //print("parent: " + pal)
        pal(0) = " " + (pal(0).trim.toInt + pal(1).trim.toInt)
        pal.remove(1)
        //println(" child: " + pal)
        buildInHelper(pal, level+1)
      }
    }
  }


  def copy(array: ArrayBuffer[String]):ArrayBuffer[String] ={
    var aB: ArrayBuffer[String] = new ArrayBuffer[String]
    for(i <- 0 to array.length-1){
      aB.append(array(i))
    }
    aB
  }
  def getArrayBuffer(array: Array[String]): ArrayBuffer[String] = {
    var aB: ArrayBuffer[String] = new ArrayBuffer[String]
    for(i <- 0 to array.length-1){
      aB.append(array(i))
    }
    aB
  }
  def test(pal:ArrayBuffer[String]): Unit = {
    var left2Pal = ""
    var right2Pal = ""
    var palCopy = new ArrayBuffer[String]
    for (i <- 1 to pal.length / 2 - 1) {
      palCopy = copy(pal)
      left2Pal = "" + (palCopy(i - 1).trim.toInt + palCopy(i).trim.toInt)
      right2Pal = "" + (palCopy(pal.length - i).trim.toInt + palCopy(pal.length - 1 - i).trim.toInt)
      palCopy.remove(i-1); palCopy(i-1) = left2Pal
      palCopy.remove(palCopy.length - i); palCopy(palCopy.length - i) = right2Pal
      //buildInHelper(palCopy, level+1)
      println(palCopy)
    }
  }
}

object Main{
  def main(args: Array[String]): Unit ={
    /*var pan = ArrayBuffer("23", "4","6")
    var panCopy = pan
    pan -= "6"

    println(7/2)
    println(pan.toString().substring(12,pan.toString().length-1).replaceAll(",",""))
    println(panCopy)*/


    var pan:GeneratePalindrome = new GeneratePalindrome
    pan.buildOut
    pan.hashTable.printHashTable()
    println(pan.hashTable.TABLE_SIZE)
    //pan.buildIn
    //println(pan.palindromes)
    //println(" length: " + pan.palindromes.length)

    //pan.test(ArrayBuffer("1","1","1","1","1","1"))
  }
}

