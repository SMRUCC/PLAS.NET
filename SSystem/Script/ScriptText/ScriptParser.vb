#Region "Microsoft.VisualBasic::bd5e8e5358a2a0501cf2d81689d490f2, sub-system\PLAS.NET\SSystem\Script\ScriptParser.vb"

' Author:
' 
'       asuka (amethyst.asuka@gcmodeller.org)
'       xie (genetics@smrucc.org)
'       xieguigang (xie.guigang@live.com)
' 
' Copyright (c) 2018 GPL3 Licensed
' 
' 
' GNU GENERAL PUBLIC LICENSE (GPL3)
' 
' 
' This program is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.
' 
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
' 
' You should have received a copy of the GNU General Public License
' along with this program. If not, see <http://www.gnu.org/licenses/>.



' /********************************************************************************/

' Summaries:

'     Module ScriptParser
' 
'         Function: (+2 Overloads) ConstantParser, ExperimentParser, ParseFile, ParseScript, ParseStream
'                   sEquationParser
' 
' 
' /********************************************************************************/

#End Region

Imports System.IO
Imports System.Runtime.CompilerServices
Imports Microsoft.VisualBasic.ComponentModel.DataSourceModel
Imports Microsoft.VisualBasic.Linq
Imports Microsoft.VisualBasic.Math.Scripting.MathExpression
Imports SMRUCC.genomics.Analysis.SSystem.Kernel.ObjectModels

Namespace Script

    Public Module ScriptParser

        ''' <summary>
        ''' 
        ''' </summary>
        ''' <param name="scriptText">脚本的文本内容</param>
        ''' <returns></returns>
        ''' 
        <Extension>
        Public Function ParseScript(scriptText As String) As Model
            Dim tokens As ScriptToken() = TokenIcer.TryParse(scriptText.LineTokens)
            Dim typeTokens = (From x As ScriptToken
                              In tokens
                              Select x
                              Group x By x.name Into Group) _
                                   .ToDictionary(Function(x) x.name,
                                                 Function(x)
                                                     Return x.Group.ToArray
                                                 End Function)

            Dim equations = typeTokens(Script.ScriptTokens.Reaction).Select(AddressOf sEquationParser)
            Dim Disturbs As Experiment()
            Dim FinalTime As Integer
            Dim val As New ExpressionEngine

            Dim c =
                If(typeTokens.ContainsKey(Script.ScriptTokens.Constant),
                typeTokens(Script.ScriptTokens.Constant).Select(AddressOf ModelParsers.ConstantParser),
                {})

            For Each x As NamedValue(Of String) In c
                Call val.SetSymbol(x.Name, x.Value)
            Next

            Dim inits = typeTokens(Script.ScriptTokens.InitValue).Select(Function(x) var.TryParse(x.Text, val))

            If typeTokens.ContainsKey(Script.ScriptTokens.Disturb) Then
                Disturbs = typeTokens(Script.ScriptTokens.Disturb).Select(Function(x) ExperimentParser(x.Text))
            Else
                Disturbs = {}
            End If

            If Not typeTokens.ContainsKey(Script.ScriptTokens.Time) Then
                FinalTime = 100
            Else
                FinalTime = val.Evaluate(typeTokens(Script.ScriptTokens.Time).First.Text)
            End If

            Dim Title As String

            If Not typeTokens.ContainsKey(Script.ScriptTokens.Title) Then
                Title = "UNNAMED TITLE"
            Else
                Title = typeTokens(Script.ScriptTokens.Title).First.Text
            End If

            Dim Comments As String() =
                If(typeTokens.ContainsKey(Script.ScriptTokens.Comment),
                typeTokens(Script.ScriptTokens.Comment).Select(Function(x) x.Text),
                {})

            Dim model As New Model With {
                .sEquations = equations,
                .Vars = inits,
                .Experiments = Disturbs,
                .Comment = Comments.JoinBy(vbCrLf),
                .FinalTime = FinalTime,
                .Title = Title,
                .Constant = c
            }
            Dim NameList As String()

            If typeTokens.ContainsKey(Script.ScriptTokens.Alias) Then
                NameList = typeTokens(Script.ScriptTokens.Alias).Select(Function(x) x.Text)
            Else
                NameList = {}
            End If

            For Each s As String In NameList
                s = Mid(s, 7)
                Dim Name As String = s.Split.First
                model.FindObject(s).Title = Mid(s, Len(Name) + 2)
            Next

            Dim sc As IEnumerable(Of String) =
                If(typeTokens.ContainsKey(Script.ScriptTokens.SubsComments),
                typeTokens(Script.ScriptTokens.SubsComments).Select(Function(x) x.Text),
                {})

            For Each s As String In sc
                s = Mid(s, 9)
                Dim Name As String = s.Split.First
                model.FindObject(Name).Comment = Mid(s, Len(Name) + 2)
            Next

            model.UserFunc =
                If(typeTokens.ContainsKey(Script.ScriptTokens.Function),
                typeTokens(Script.ScriptTokens.Function).Select(Function(x) CType(x.Text, [Function])),
                {})

            Return model
        End Function

        ''' <summary>
        ''' 从文件指针或者网络数据之中解析出脚本模型
        ''' </summary>
        ''' <param name="s"></param>
        ''' <returns></returns>
        <Extension>
        Public Function ParseStream(s As Stream) As Model
            Return ParseScript(New StreamReader(s).ReadToEnd)
        End Function

        <Extension>
        Public Function ParseFile(path As String) As Model
            Return ParseScript(path.ReadAllText)
        End Function
    End Module
End Namespace
