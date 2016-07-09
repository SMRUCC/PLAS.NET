﻿#Region "Microsoft.VisualBasic::5177f22f612b958781b4cf32c0aa2c6f, ..\GCModeller\sub-system\PLAS.NET\SSystem\Compiler.vb"

' Author:
' 
'       asuka (amethyst.asuka@gcmodeller.org)
'       xieguigang (xie.guigang@live.com)
' 
' Copyright (c) 2016 GPL3 Licensed
' 
' 
' GNU GENERAL PUBLIC LICENSE (GPL3)
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

#End Region

Imports Microsoft.VisualBasic.ComponentModel.Collection.Generic
Imports SMRUCC.genomics.Analysis.SSystem

Public Module Compiler

    Public Delegate Function ICompiler(input As String, out As String, autoFix As Boolean) As Boolean

    ''' <summary>
    ''' 格式名称的大小写不敏感
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property Compilers As IReadOnlyDictionary(Of String, ICompiler) =
        New HashDictionary(Of ICompiler) From {
 _
        {"script", AddressOf ScriptCompiler},
        {"sbml", AddressOf SBMLCompiler}
    }

    Public Function ScriptCompiler(input As String, out As String, autoFix As Boolean) As Boolean
        Return Script.ScriptCompiler.Compile(input, autoFix).Save(out)
    End Function

    Public Function SBMLCompiler(input As String, out As String, autoFix As Boolean) As Boolean
        Return SBML.Compile(input, autoFix).Save(out)
    End Function
End Module
