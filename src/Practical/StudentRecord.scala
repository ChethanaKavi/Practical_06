object StudentRecord{
  import scala.io.StdIn._

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || totalMarks < 0) {
      (false, Some("Marks and total marks must be positive integers."))
    } else if (marks > totalMarks) {
      (false, Some("Marks cannot exceed total possible marks."))
    } else {
      (true, None)
    }
  }

  def calculateGrade(percentage: Double): Char = {
    percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
  }

  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter student name:")
    val name = readLine()

    println("Enter marks obtained:")
    val marks = readInt()

    println("Enter total possible marks:")
    val totalMarks = readInt()

    val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
    if (!isValid) {
      println(s"Invalid input: ${errorMessage.get}")
      getStudentInfo() // Retry if invalid
    } else {
      val percentage = (marks.toDouble / totalMarks) * 100
      val grade = calculateGrade(percentage)
      (name, marks, totalMarks, percentage, grade)
    }
  }

  def printStudentRecord(student: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = student
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(s"Percentage: $percentage%")
    println(s"Grade: $grade")
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'D')

    while (!isValid) {
      println("Enter student name:")
      val name = readLine()

      println("Enter marks obtained:")
      val marks = readInt()

      println("Enter total possible marks:")
      val totalMarks = readInt()

      // Validate input
      val validation = validateInput(name, marks, totalMarks)
      if (validation._1) {
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = calculateGrade(percentage)
        studentInfo = (name, marks, totalMarks, percentage, grade)
        isValid = true
      } else {
        println(s"Invalid input: ${validation._2.get}")
      }
    }
    studentInfo
  }

  def main(args: Array[String]): Unit = {
    // Get student info with retry until valid input is provided
    val student = getStudentInfoWithRetry()

    // Print the student record
    printStudentRecord(student)
  }
}
