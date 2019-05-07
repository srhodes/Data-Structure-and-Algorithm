/*
* srhodes17@georgefox.edu
* Program4
* 2019-4-13
* */

package AdvancedScala


/**
  * used for multiple inheritance in Scala
  */
trait CompareTo
{
  // returning student age
  def compareTo(student1: Student, student2: Student): Int
}


/**
  * implementation from trait compareto
  */
object CompareName extends  CompareTo
{
  /**
    * Method of comparison
    *
    * @param student1 - first instance of student
    * @param student2 - second instance of student
    * @return
    */
  def compareTo(student1:Student, student2: Student): Int =
  {
    // returning student age
      return student1.name.compareTo(student2.name)
  }
}


/**
  * implementation from trait compareto
  */
object CompareAge extends CompareTo
{
  // age calculation between students
  def compareTo(student1:Student, student2: Student): Int =
  {
      return student1.age - student2.age
  }
}

/**
  * implementation from trait compareto
  */

object CompareGpa extends CompareTo
{
  // gpa calculation between students
  def compareTo(student1: Student, student2: Student): Int =
  {
    return ((student1.gpa - student2.gpa) * 1000).toInt
  }
}

/**
  * List of students that include every methods to
  * manipulate the list of students
  */

class StudentList()
{
  var size = 0
  var students: Array[Student] = new Array[Student](51)


  /**
    * Method of adding student in front
    *
    * @param student - instance of student
    */
  def append(student: Student): Unit =
  {
    students(size) = student
    size = size + 1
  }

  /**
    * Method of adding student in any location
    *
    * @param location - location of student
    * @param student - instance of student
    */

  def insert(location: Int, student: Student):Unit =
  {
    // repetition and selection structure used to create insert function
    if(location < 0 || location > size)
    {
      return
    }
    for(i <- size - 1 until location)
    {
      students(i+1) = students(i)
    }
    students(location) = student
  }

  /**
    * Method to remove student
    *
    * @param location - location to remove student
    */
  def remove(location: Int): Unit =
  {
    // repetition and selection structure used to create remove function
      if(location < 0 || location >= size)
      {
        return
      }
      for(i <- location until size - 2)
      {
        students(i) = students(i+1)
      }
      students(size-1) = null
      size = size - 1
  }

  /**
    * Method to get the size of students
    *
    * @return
    */
  def length(): Int =
  {
     size
  }

  /**
    *
    * @param location
    * @return
    */

  def get(location: Int): Student =
  {
    if(location < 0 || location >= size)
    {
      return null
    }

    return students(location)
  }

  /**
    *
    * @param location1
    * @param location2
    */
  def swap(location1: Int, location2: Int): Unit =
  {
    //assigning students in different locations so that swap can happen
    var student: Student = students(location1)
    students(location1) = students(location2)
    students(location2) = student
  }

  def print(): Unit =
  {
    for(i <- 0 until length())
    {
      println(get(i).name)
      println(get(i).age)
      println(get(i).gpa)
    }
  }
}

/**
  * Class of Student
  *
  * @param namec - name of students
  * @param agec - age of students
  * @param gpac - gpa of students
  */
class Student(namec: String, agec: Int, gpac: Float)
{
    var name: String = namec
    var age: Int = agec
    var gpa: Float = gpac
}

/**
  * Object containing the main function and bubble sort implementation
  */
object DataStructure
{

  /**
    * Printing all the students and catching any out-of-bound exception
    *
    * @param args
    */
   def main(args: Array[String]): Unit =
   {

     try{
       //var students: Array[Student] = new Array[Student](10)
       var students: StudentList = new StudentList

       var st = new Student("Stephan", 35, 3.8f)
       students.append(st)
       st = new Student("Christian", 33, 3.9f)
       students.append(st)
       st = new Student("Michael", 31, 3.3f)
       students.append(st)
       st = new Student("Cesar", 25, 3.7f)
       students.append(st)
       st = new Student("Oscar", 25, 3.69f)
       students.append(st)
       st = new Student("Rachel", 21, 2.3f)
       students.append(st)
       st = new Student("Lindsay", 39, 4.0f)
       students.append(st)
       st = new Student("Jen", 27, 2.7f)
       students.append(st)
       st = new Student("Denise", 18, 2.2f)
       students.append(st)
       st = new Student("Ashley", 19, 3.5f)
       students.append(st)
       st = new Student("Bruce", 20, 3.0f)
       students.append(st)


       println("Unsorted Students")
       students.print()

       bubbleSort(students,CompareName)
       println("--------------------------------------------------")
       println("Sorted by Name")
       students.print()
       bubbleSort(students, CompareAge)
       println("--------------------------------------------------")
       println("Sorted by Age")
       students.print()
       bubbleSort(students, CompareGpa)
       println("--------------------------------------------------")
       println("Sorted by Gpa")
       students.print()

     }catch
       {
       case ex: IndexOutOfBoundsException =>
       {
         println("Too many Students.")
       }
       }

  }

  /**
    * Sorting of Students
    *
    * This method of sorting is the simplest but it is known
    * to be the least effective
    */


  def bubbleSort(a: StudentList,cmp: CompareTo):Unit =
  {
    /* nested for loop combining with selection structure
     which swap the students if their location has been swap
    */

    for(i<- 0 until a.length-1)
    {
      for(j<-0 until a.length-1-i)
      {
        if(cmp.compareTo(a.get(j), a.get(j+1)) > 0)
        {
          a.swap(j, j+1)
        }
      }
    }

   // bubbleSort(students)
   // st.name();
  }
}






