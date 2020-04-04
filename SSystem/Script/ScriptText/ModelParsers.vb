﻿Imports Microsoft.VisualBasic.ComponentModel.DataSourceModel
Imports Microsoft.VisualBasic.Language
Imports SMRUCC.genomics.Analysis.SSystem.Kernel.ObjectModels

Namespace Script

    Module ModelParsers

        ''' <summary>
        ''' 解析系统方程表达式
        ''' </summary>
        ''' <param name="x"></param>
        ''' <returns></returns>
        Public Function sEquationParser(x As ScriptToken) As SEquation
            Dim value = x.text.GetTagValue("=")

            Return New SEquation With {
                .x = value.Name,
                .Expression = value.Value
            }
        End Function

        ''' <summary>
        ''' 解析出系统的状态扰动实验表达式
        ''' </summary>
        ''' <param name="line"></param>
        ''' <returns></returns>
        Public Function ExperimentParser(line As String) As Experiment
            Dim tokens As String() = line.Split
            Dim cfgs As New Dictionary(Of String, String)
            Dim disturb As New Experiment

            For i As Integer = 1 To tokens.Length - 2 Step 2
                cfgs.Add(tokens(i), tokens(i + 1))
            Next

            disturb.Start = Val(cfgs("START"))
            disturb.Interval = Val(cfgs("INTERVAL"))
            disturb.Kicks = Val(cfgs("KICKS"))
            disturb.Id = cfgs("OBJECT")
            Dim Value As String = cfgs("VALUE")

            If InStr(Value, "++") = 1 Then
                disturb.DisturbType = Types.Increase
                disturb.Value = Val(Mid(Value, 3))
            ElseIf InStr(Value, "--") = 1 Then
                disturb.DisturbType = Types.Decrease
                disturb.Value = Val(Mid(Value, 3))
            Else
                disturb.DisturbType = Types.ChangeTo
                disturb.Value = Val(Value)
            End If

            Return disturb
        End Function

        ''' <summary>
        ''' 函数会自动去除掉表达式末尾的注释
        ''' </summary>
        ''' <param name="expr"></param>
        ''' <returns></returns>
        Public Function ConstantParser(expr As Value(Of String)) As NamedValue(Of String)
            Dim name As String = (expr = (+expr).Trim).Split.First

            expr.Value = Mid(expr.Value, name.Length + 1).Trim
            expr = expr.Value _
                .GetTagValue("#", failureNoName:=False).Name _
                .GetTagValue("'", failureNoName:=False).Name _
                .GetTagValue("//", failureNoName:=False).Name

            Return New NamedValue(Of String) With {
                .Value = expr,
                .Name = name
            }
        End Function

        ''' <summary>
        ''' 这里只是进行解析，并没有立即进行求值
        ''' </summary>
        ''' <param name="expr"></param>
        ''' <returns></returns>
        Public Function ConstantParser(expr As ScriptToken) As NamedValue(Of String)
            Return ConstantParser(expr.text)
        End Function
    End Module
End Namespace