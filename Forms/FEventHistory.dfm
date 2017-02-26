object FormEventHistory: TFormEventHistory
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Event history'
  ClientHeight = 263
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object StringGrid1: TStringGrid
    Left = 32
    Top = 16
    Width = 489
    Height = 137
    ColCount = 3
    FixedCols = 0
    TabOrder = 0
    ColWidths = (
      110
      116
      244)
  end
  object Button1: TButton
    Left = 136
    Top = 218
    Width = 81
    Height = 25
    Caption = 'Refresh'
    TabOrder = 1
  end
  object Button2: TButton
    Left = 312
    Top = 218
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
