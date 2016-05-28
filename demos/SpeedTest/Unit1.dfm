object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 64
    Top = 48
    Width = 97
    Height = 17
    Caption = 'Multicore'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 64
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Go!'
    TabOrder = 1
    OnClick = Button1Click
  end
end
