import scala.collection.mutable.ArrayBuffer

class GeneratePalindrome {
  var n:Int = 20

  var m:Int = 2
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

  def removeIndexes(array:ArrayBuffer[String], indexes: Array[Int]): Unit ={//removes the specified indexes in the array
    if(indexes==null)
      return//don't remove anything
    for(i <- 0 to indexes.length-1){
      array.remove(indexes(i))//remove specified indexes of array
    }
  }
  def restore(array:ArrayBuffer[String], indexes: Array[Int], value: String): Unit ={
    for(i <- 0 to indexes.length-1){
      array(indexes(i)) = value//remove specified indexes of array
    }
  }

  def buildIn(pal:String): Unit ={
    val lastPal = getArrayBuffer(pal.split(" "))
    //var lastPal = getArrayBuffer(palindromes(palindromes.length-1).split(" "))//get last palindrome in list of palindromes
    buildInHelper(lastPal, 1, null)
  }
  def buildInHelper(pal:ArrayBuffer[String], level:Int, removeIndexe: Array[Int]): Unit ={
    //var pal = getArrayBuffer(palindrome.split(" ").array)//get all nums in the palindrome (in an array)
    val palCopy = pal.clone()//copy(pal)
    //println("Level: " + level + " pal: " + palCopy)
    removeIndexes(palCopy, removeIndexe)//remove specified indexes from palCopy
    println("pal after remove: " + palCopy)
    val palStr = palCopy.toString().substring(12,palCopy.toString().length-1).replaceAll(",","")
    if(palCopy.length<=1)//base case
      return
    if(hashTable.searchTable(palStr) && level!=1)//palindrome already in hashTable and accounted for
      return

    if(level!=1) {
      //var palStr = pal.toString().substring(12,pal.toString().length-1).replaceAll(",","")
      if(palStr.contains(" "+m+ " ") || palStr.contains(""+m+" "))//add only palindromes with the number m
        hashTable.add(palStr)//pal.toString().substring(12,pal.toString().length-1).replaceAll(",",""))
      /*
      var inPals = false//will be true if value in palindromes
      for(i <- 0 to palindromes.length-1){
        if(palindromes(i).equals(pal.toString().substring(12,pal.toString().length-1).replaceAll(",","")))//know this is lazy coding sorry, all I did was remove "ArrayBuffer" and ")" from the string
          inPals = true
      }
      if(!inPals)
        palindromes.append(pal.toString().substring(12,pal.toString().length-1).replaceAll(",",""))//append to list if this palindrome isn't already in it*/
    }
    //start parsing tree
    if(palCopy.length>3){
      if(palCopy.length%2==0){//the length of the panlindrome is even
        var midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        //palCopy.remove(midPoint)//handle middle two nums first
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint).trim.toInt)
        buildInHelper(palCopy, level+1, Array(midPoint+1))

        //restore palCopy to orginal
        restore(palCopy, Array(midPoint), midVal)

        //do it for rest of pal
        var left2Pal = ""
        var right2Pal = ""
        var prevIndValue = ""
        for(i <- 1 to palCopy.length/2-1){
          //palCopy = pal.clone()//copy(pal)
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(palCopy.length-i).trim.toInt  + palCopy(palCopy.length-1-i).trim.toInt)
          prevIndValue = palCopy(i-1)
          /*palCopy.remove(i-1);*/ palCopy(i-1) = left2Pal
          /*palCopy.remove(palCopy.length - i);*/ palCopy(palCopy.length - i) = right2Pal
          //println("parent: " + pal + " child: " + palCopy)
          //println("palCopy in even before Build: " + palCopy)
          buildInHelper(palCopy, level+1, Array(palCopy.length-i-1, i))
          //println("palCopy in even after Build: " + palCopy)
          restore(palCopy, Array(palCopy.length-i, i-1), prevIndValue)
        }
      }
      else{//pal is odd
        //println("original pal: " + palCopy + " Level: " + level)
        val midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        val OGMidVal = midVal//orignal mid value
        //palCopy.remove(midPoint) //-= palCopy(midPoint)//handle middle three nums first
        midVal = "" + (midVal.trim.toInt + palCopy(midPoint+1).trim.toInt)
        //palCopy.remove(midPoint) //-= palCopy(midPoint)
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint+2).trim.toInt)
        //println("parent: " + pal + " child: " + palCopy)
        buildInHelper(palCopy, level+1, Array(midPoint+1, midPoint+1))

        //restore
        //println("before restore: " + palCopy + " Level: " + level)
        //var beforeRestore = palCopy.toString()
        restore(palCopy, Array(midPoint), OGMidVal)
        //println("after restore: " + palCopy + " Level: " + level)// good up to this point

        //var afterRestore = palCopy.toString()
        //do it for rest of pal
        var left2Pal = ""
        var right2Pal = ""
        var prevIndValue = ""
        /*
        //split up for loop amongst mutiple threads.
        // example... if loop goes from 0 to 10 and there are 3 threads, each thread handles 1/3
         */
        for(i <- 1 to palCopy.length/2-1){
          //println("in odd palCopy before right2Pal: " + palCopy + " after restore: " + afterRestore + " before restore: " + beforeRestore + " original pal: " + palStr)
          //palCopy = pal.clone()//copy(pal)
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(palCopy.length-i).trim.toInt + palCopy(palCopy.length-1-i).trim.toInt)
          //println("left2pal: " + left2Pal +  " right2Pal:" + right2Pal)
          prevIndValue = palCopy(i-1)
          /*palCopy.remove(i-1);*/ palCopy(i-1) = left2Pal
          /*palCopy.remove(palCopy.length-i);*/ palCopy(palCopy.length-i)=right2Pal
          //println("parent: " + pal + " child: " + palCopy)
          //println("pal before build call: " + palCopy)
          buildInHelper(palCopy, level+1, Array(palCopy.length-i-1, i))//Array(i, palCopy.length-i+1))
          //println("pal before bottom restore: " + palCopy)
          restore(palCopy, Array(palCopy.length-i, i-1)/*Array(i-1, palCopy.length-i-1)*/, prevIndValue)
          //println("after bottom restore: " + palCopy)
        }
      }
    }
    else{//pal.length is less than 3
      if(palCopy.length == 3){
        //print("parent: " + pal)
        palCopy(0) = " " + (palCopy(0).trim.toInt + palCopy(1).trim.toInt + palCopy(2).trim.toInt)
        palCopy.remove(1)
        palCopy.remove(1)
        //println(" child: " + pal)
        buildInHelper(palCopy, level+1, null)
      }
      else{//pal.length == 2
        //print("parent: " + pal)
        palCopy(0) = " " + (palCopy(0).trim.toInt + palCopy(1).trim.toInt)
        palCopy.remove(1)
        //println(" child: " + pal)
        buildInHelper(palCopy, level+1, null)
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
  def removeIndexes(array:ArrayBuffer[String], indexes: Array[Int]): Unit ={//removes the specified indexes in the array
    if(indexes==null)
      return//don't remove anything
    for(i <- 0 to indexes.length-1){
      array.remove(indexes(i))//remove specified indexes of array
    }
  }

  def restore(array:ArrayBuffer[String], indexes: Array[Int], value: String): Unit ={
    for(i <- 0 to indexes.length-1){
      array(indexes(i)) = value//remove specified indexes of array
    }
  }

  def main(args: Array[String]): Unit ={
    /*var pan = ArrayBuffer("23", "4","6")
    var panCopy = pan
    pan -= "6"

    println(7/2)
    println(pan.toString().substring(12,pan.toString().length-1).replaceAll(",",""))
    println(panCopy)*/




    /*var buffer = ArrayBuffer("hello","why","cheese", "bye")
    println(buffer)
    //removeIndexes(buffer, Array(0,1))
    //println(buffer)
    restore(buffer, Array(0,1), "cheap")
    println(buffer)*/

    var pan:GeneratePalindrome = new GeneratePalindrome
    pan.buildOut
    pan.hashTable.printHashTable()
    println(pan.hashTable.TABLE_SIZE)
    println(pan.hashTable.tableCount-2)
    //pan.buildIn
    //println(pan.palindromes)
    //println(" length: " + pan.palindromes.length)

    //pan.test(ArrayBuffer("1","1","1","1","1","1"))
  }
}

