object FormUserDigitalKeyboard: TFormUserDigitalKeyboard
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Input value'
  ClientHeight = 336
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 6
    Top = 56
    Width = 65
    Height = 65
    Caption = '1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 76
    Top = 56
    Width = 65
    Height = 65
    Caption = '2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 6
    Top = 125
    Width = 65
    Height = 65
    Caption = '4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 145
    Top = 56
    Width = 65
    Height = 65
    Caption = '3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 76
    Top = 125
    Width = 65
    Height = 65
    Caption = '5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 146
    Top = 125
    Width = 65
    Height = 65
    Caption = '6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 6
    Top = 194
    Width = 65
    Height = 65
    Caption = '7'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 76
    Top = 194
    Width = 65
    Height = 65
    Caption = '8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 146
    Top = 194
    Width = 65
    Height = 65
    Caption = '9'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = Button9Click
  end
  object ButtonCLR: TButton
    Left = 216
    Top = 56
    Width = 65
    Height = 65
    Caption = 'CLR'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = ButtonCLRClick
  end
  object ButtonDel: TButton
    Left = 216
    Top = 125
    Width = 65
    Height = 65
    Caption = 'DEL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
    OnClick = ButtonDelClick
  end
  object ButtonEnter: TButton
    Left = 146
    Top = 263
    Width = 135
    Height = 65
    Caption = 'ENTER'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 11
    OnClick = ButtonEnterClick
  end
  object ButtonEsc: TButton
    Left = 216
    Top = 194
    Width = 65
    Height = 65
    Caption = 'ESC'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 12
    OnClick = ButtonEscClick
  end
  object ButtonDecimalSeparator: TButton
    Left = 76
    Top = 263
    Width = 65
    Height = 65
    Caption = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -45
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 13
    OnClick = ButtonDecimalSeparatorClick
  end
  object Button0: TButton
    Left = 6
    Top = 263
    Width = 65
    Height = 65
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 14
    OnClick = Button0Click
  end
  object EditResult: TEdit
    Left = 6
    Top = 8
    Width = 275
    Height = 41
    BiDiMode = bdLeftToRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -29
    Font.Name = 'Arial'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 15
    Text = '0'
    OnClick = EditResultClick
  end
end
