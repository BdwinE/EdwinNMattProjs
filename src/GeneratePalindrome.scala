import scala.collection.mutable.ArrayBuffer

class GeneratePalindrome {
  var palindromes:ArrayBuffer[String] = _//new ArrayBuffer[String]//list of palindromes
  var hashTable: HashTable = _//new HashTable(n)

  def init(n:Int, m:Int): Unit ={//builds
    palindromes = new ArrayBuffer[String]
    hashTable = new HashTable(n)
    hashTable.m = m
    var pal = ""
    for(i <- 0 to n-1){
      pal += " 1"
    }
    hashTable.add(n+"")
    hashTable.add(pal.substring(1))
    buildIn(pal.substring(1))
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
    buildInHelper(lastPal, 1, null)
  }
  def buildInHelper(pal:ArrayBuffer[String], level:Int, removeIndexe: Array[Int]): Unit ={
    val palCopy = pal.clone()//copy(pal)
    removeIndexes(palCopy, removeIndexe)//remove specified indexes from palCopy
    val palStr = palCopy.toString().substring(12,palCopy.toString().length-1).replaceAll(",","")
    if(palCopy.length<=1)//base case
      return
    if(hashTable.searchTable(palStr) && level!=1)//palindrome already in hashTable and accounted for
      return

    if(level!=1) {
      hashTable.add(palStr)
    }
    //start parsing tree
    if(palCopy.length>3){
      if(palCopy.length%2==0){//the length of the palindrome is even
        var midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint).trim.toInt)
        buildInHelper(palCopy, level+1, Array(midPoint+1))

        //restore palCopy to orginal
        restore(palCopy, Array(midPoint), midVal)

        //do it for rest of pal
        var left2Pal = ""
        var right2Pal = ""
        var prevIndValue = ""
        for(i <- 1 to palCopy.length/2-1){
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(palCopy.length-i).trim.toInt  + palCopy(palCopy.length-1-i).trim.toInt)
          prevIndValue = palCopy(i-1)
          palCopy(i-1) = left2Pal
          palCopy(palCopy.length - i) = right2Pal
          buildInHelper(palCopy, level+1, Array(palCopy.length-i-1, i))
          restore(palCopy, Array(palCopy.length-i, i-1), prevIndValue)
        }
      }
      else{//pal is odd
        val midPoint = palCopy.length/2-1
        var midVal = palCopy(midPoint)
        val OGMidVal = midVal//orignal mid value
        midVal = "" + (midVal.trim.toInt + palCopy(midPoint+1).trim.toInt)
        palCopy(midPoint) = "" + (midVal.trim.toInt + palCopy(midPoint+2).trim.toInt)
        buildInHelper(palCopy, level+1, Array(midPoint+1, midPoint+1))

        //restore
        restore(palCopy, Array(midPoint), OGMidVal)

        //do it for rest of pal
        var left2Pal = ""
        var right2Pal = ""
        var prevIndValue = ""
        for(i <- 1 to palCopy.length/2-1){
          left2Pal = "" + (palCopy(i-1).trim.toInt + palCopy(i).trim.toInt)
          right2Pal = "" + (palCopy(palCopy.length-i).trim.toInt + palCopy(palCopy.length-1-i).trim.toInt)
          prevIndValue = palCopy(i-1)
          palCopy(i-1) = left2Pal
          palCopy(palCopy.length-i)=right2Pal
          buildInHelper(palCopy, level+1, Array(palCopy.length-i-1, i))
          restore(palCopy, Array(palCopy.length-i, i-1), prevIndValue)
        }
      }
    }
    else{//pal.length is less than 3
      if(palCopy.length == 3){
        palCopy(0) = " " + (palCopy(0).trim.toInt + palCopy(1).trim.toInt + palCopy(2).trim.toInt)
        palCopy.remove(1)
        palCopy.remove(1)
        buildInHelper(palCopy, level+1, null)
      }
      else{//pal.length == 2
        palCopy(0) = " " + (palCopy(0).trim.toInt + palCopy(1).trim.toInt)
        palCopy.remove(1)
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
      println(palCopy)
    }
  }
}

object Main{
  def main(args: Array[String]): Unit ={
    val n = 15//TODO: get n from user input...
    val m = 2//TODO: get m from user input...
    var time = System.currentTimeMillis()
    val pan:GeneratePalindrome = new GeneratePalindrome
    pan.init(n,m)
    time = System.currentTimeMillis()-time
    println("Number of palindromic sequences found: " + pan.hashTable.palWithMTop)
    println("It took me " + time/1000 + "s")
  }
}

