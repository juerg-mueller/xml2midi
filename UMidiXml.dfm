object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'XML to MIDI Converter'
  ClientHeight = 347
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 216
    Top = 136
    Width = 301
    Height = 45
    Caption = 'Drop .xml file here'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -37
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object XMLDocument1: TXMLDocument
    Left = 272
    Top = 216
  end
end
