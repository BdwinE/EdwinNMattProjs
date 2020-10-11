
class HashTable(n:Int) {
  //use a hashtable to access and store data instead of array buffer
  final val TABLE_SIZE = determineSize(n)//this hash table's size does not change... sorry
  var hashTable:Array[Node] = new Array[Node](TABLE_SIZE)
  var palWith_m:Array[String]= _//contains all palindromes with the value m
  var palWithMTop = 0
  var m:Int = _
  var tableCount = 0//number of values in table




  def add(str:String): Unit ={//add string to hash table
    val index = hashFunction(str, TABLE_SIZE)
    if(hashTable(index)==null){
      hashTable(index) = new Node(str)
      tableCount += 1
      if(contains(str, m)) {
        palWith_m(palWithMTop) = str
        palWithMTop += 1
      }
    }
    else{
      var pointer:Node = hashTable(index)
      if(pointer.value.equals(str))
        return//value is already in hashtable
      while(pointer.next!=null){
        if(pointer.next.value.equals(str))
          return//value is already in hashtable... to avoid adding duplicates
        pointer = pointer.next
      }
      pointer.next = new Node(str)
      tableCount += 1
      if(contains(str, m)) {
        palWith_m(palWithMTop) = str
        palWithMTop += 1
      }
    }

  }
  def searchTable(str: String): Boolean = {//searches the table to see if value is in it
    var index = hashFunction(str, TABLE_SIZE)
    var pointer:Node = hashTable(index)
    while(pointer != null){
      if(pointer.value.equals(str))
        return true
      pointer = pointer.next
    }
    return false
  }
  def hashFunction(key: String, tableSize : Int): Int ={
    //add the first index with the last index and then multiply that by the middle index
    var keyVal = key.trim
    var sum = 0
    val con = 7

    if(keyVal.length>10) {
      for (i <- 0 to 10) {
        sum += keyVal.charAt(i).toInt * Math.pow(con.toDouble, i.toDouble).toInt
      }
    }
    else{
      for (i <- 0 to keyVal.length-1) {
        sum += keyVal.charAt(i).toInt * Math.pow(con, i).toInt
      }
    }
    val index = sum % tableSize
    if(index<0) {
      -index
    }
    else {
      index
    }
  }
  def determineSize(n:Int):Int ={//used to determine optimial size for hashtable
    var size = Math.pow(2, (n/2).floor).toInt
    palWith_m = new Array[String](size)
    return ((size*2)+1)
  }
  def determineSizeHelp(n:Int, buildDirection : Boolean): Int ={
    if(n<=3)
      return 1
    println(n/2)
    if(buildDirection)
      return n/2 + determineSizeHelp(n-2, buildDirection)
    else
      return 1 + determineSizeHelp(n/2, buildDirection)
  }
  def printHashTable(): Unit ={
    var current:Node = new Node("")
    for(i <- 0 to hashTable.length-1) {
      current = hashTable(i)
      if(current!=null) {
        while (current != null) {
          println(current.value + " Index: " + i)
          current = current.next
        }
      }
    }
  }
  def contains(str:String, subStr:Int): Boolean ={
    //assumes the str string is a palindrome, only checks half of the palindrome
    var strSplit = str.split(" ")
    for(i <- 0 to strSplit.length/2){
      if(strSplit(i).toInt == subStr)
        return true
    }
    return false
  }

  class Node(str:String){
    var value:String = str
    var next:Node = null

    def setValue(valu:String): Unit ={
      value = valu
    }
    def getValue: String ={
      value
    }
    override
    def toString():String ={
      value
    }
  }
}

object Mainn{
  def main(args: Array[String]): Unit ={
    var hTable:HashTable = new HashTable(16)
    println(hTable.contains("12 2 2 12", 12))
    /*val r = scala.util.Random
    var randString = ""
    for(z <- 0 to 70) {
      randString = ""
      for (i <- 0 to r.nextInt(10)) {
        randString += r.nextPrintableChar()
      }
      hTable.add(randString)
    }
    //hTable.add(hTable.hashTable(0).toString())
    //println("6 size: " + hTable.determineSize(13))
    hTable.add("hello")
    println(hTable.searchTable("hell"))
    hTable.printHashTable()

    var index1 = hTable.hashFunction("1 1 3 1 1", 16)
    var index2 = "1 1 3 1 1".hashCode

    println(hTable.hashFunction("1 1 3 1 1", 16) + " index1: " + index1 + " index2: " + index2)// + " " + "1 2 1".hashCode)*/

  }
}
