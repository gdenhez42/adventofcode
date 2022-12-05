Imports System
Imports System.IO

Module Program
    Sub Main(args As String())
        Dim puzzleInput As String
        Using sr As New StreamReader("input.txt")
            puzzleInput = sr.ReadToEnd()
        End Using
        
        Dim cratesAndOps() As String = puzzleInput.Split(Environment.NewLine + Environment.NewLine)
        Dim cratesLines() As String = cratesAndOps(0).Split(Environment.NewLine)
        Dim nbStacks As Integer = cratesLines(cratesLines.GetUpperBound(0)).Split(" ", StringSplitOptions.RemoveEmptyEntries).Length

        ' PART 1

        ' Read where the crates are initially
        Dim stacks(nbStacks) As Stack
        For stackIndex As Integer = 0 to nbStacks-1
            stacks(stackIndex) = new Stack()
        Next
        For line As Integer = cratesLines.GetUpperBound(0)-1 to 0 Step -1
            Dim cratesLine As String = cratesLines(line)
            For stackIndex As Integer = 0 to nbStacks-1
                Dim crate As Char = cratesLine.Chars(1 + 4*stackIndex)
                If crate <> " "c Then
                    stacks(stackIndex).Push(crate)
                End If
            Next
        Next

        ' Apply the operations
        For Each operation In cratesAndOps(1).Split(Environment.NewLine)
            Dim operants() As String = operation.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            Dim nb As Integer = Convert.toInt32(operants(1))
            Dim from As Integer = Convert.toInt32(operants(3))
            Dim dest As Integer = Convert.toInt32(operants(5))

            For i As Integer = 0 to nb-1
                stacks(dest-1).Push(stacks(from-1).Pop())
            Next
        Next

        ' Show the content of the stacks -- good enough
        For stackIndex As Integer = 0 to nbStacks-1
            For Each value In stacks(stackIndex)
                Console.Write("{0} ", value)
            Next
            Console.WriteLine("")
        Next
        Console.WriteLine("")

        ' PART 2

        ' Read where the crates are initially
        Dim stacks2(nbStacks) As ArrayList
        For stackIndex As Integer = 0 to nbStacks-1
            stacks2(stackIndex) = new ArrayList()
        Next
        For line As Integer = 0 to cratesLines.GetUpperBound(0)-1
            Dim cratesLine As String = cratesLines(line)
            For stackIndex As Integer = 0 to nbStacks-1
                Dim crate As Char = cratesLine.Chars(1 + 4*stackIndex)
                If crate <> " "c Then
                    stacks2(stackIndex).Add(crate)
                End If
            Next
        Next

        ' Apply the operations
        For Each operation In cratesAndOps(1).Split(Environment.NewLine)
            Dim operants() As String = operation.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            Dim nb As Integer = Convert.toInt32(operants(1))
            Dim from As Integer = Convert.toInt32(operants(3))
            Dim dest As Integer = Convert.toInt32(operants(5))

            stacks2(dest-1).InsertRange(0, stacks2(from-1).GetRange(0, nb))
            stacks2(from-1).RemoveRange(0, nb)
        Next

        ' Show the content of the stacks -- good enough
        For stackIndex As Integer = 0 to nbStacks-1
            For Each value In stacks2(stackIndex)
                Console.Write("{0} ", value)
            Next
            Console.WriteLine("")
        Next

    End Sub
End Module
