object FormTemperatureRanges: TFormTemperatureRanges
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Temperature Ranges'
  ClientHeight = 230
  ClientWidth = 578
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
  object LabelSection1: TLabel
    Left = 23
    Top = 20
    Width = 61
    Height = 17
    Caption = 'Section 1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection2: TLabel
    Left = 23
    Top = 49
    Width = 61
    Height = 17
    Caption = 'Section 2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection4: TLabel
    Left = 23
    Top = 107
    Width = 61
    Height = 17
    Caption = 'Section 4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection3: TLabel
    Left = 23
    Top = 78
    Width = 61
    Height = 17
    Caption = 'Section 3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection5: TLabel
    Left = 23
    Top = 137
    Width = 61
    Height = 17
    Caption = 'Section 5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection6: TLabel
    Left = 305
    Top = 20
    Width = 61
    Height = 17
    Caption = 'Section 6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection7: TLabel
    Left = 305
    Top = 49
    Width = 61
    Height = 17
    Caption = 'Section 7'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection9: TLabel
    Left = 305
    Top = 107
    Width = 61
    Height = 17
    Caption = 'Section 9'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection8: TLabel
    Left = 305
    Top = 78
    Width = 61
    Height = 17
    Caption = 'Section 8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabelSection10: TLabel
    Left = 305
    Top = 137
    Width = 69
    Height = 17
    Caption = 'Section 10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object LabeledEditSection1MinYValue: TLabeledEdit
    Left = 140
    Top = 16
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 0
    Text = '90'
  end
  object LabeledEditSection1MaxYValue: TLabeledEdit
    Left = 234
    Top = 16
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 1
    Text = '110'
  end
  object LabeledEditSection2MinYValue: TLabeledEdit
    Left = 140
    Top = 45
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 2
    Text = '100'
  end
  object LabeledEditSection2MaxYValue: TLabeledEdit
    Left = 234
    Top = 45
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 3
    Text = '120'
  end
  object LabeledEditSection3MinYValue: TLabeledEdit
    Left = 140
    Top = 74
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 4
    Text = '110'
  end
  object LabeledEditSection3MaxYValue: TLabeledEdit
    Left = 234
    Top = 74
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 5
    Text = '130'
  end
  object LabeledEditSection4MinYValue: TLabeledEdit
    Left = 140
    Top = 103
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 6
    Text = '120'
  end
  object LabeledEditSection4MaxYValue: TLabeledEdit
    Left = 234
    Top = 103
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 7
    Text = '140'
  end
  object LabeledEditSection5MinYValue: TLabeledEdit
    Left = 140
    Top = 132
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 8
    Text = '130'
  end
  object LabeledEditSection5MaxYValue: TLabeledEdit
    Left = 234
    Top = 132
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 9
    Text = '150'
  end
  object LabeledEditSection6MinYValue: TLabeledEdit
    Left = 422
    Top = 16
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 10
    Text = '120'
  end
  object LabeledEditSection6MaxYValue: TLabeledEdit
    Left = 517
    Top = 16
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 11
    Text = '140'
  end
  object LabeledEditSection7MinYValue: TLabeledEdit
    Left = 422
    Top = 45
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 12
    Text = '110'
  end
  object LabeledEditSection7MaxYValue: TLabeledEdit
    Left = 517
    Top = 45
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 13
    Text = '130'
  end
  object LabeledEditSection8MinYValue: TLabeledEdit
    Left = 422
    Top = 74
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 14
    Text = '100'
  end
  object LabeledEditSection8MaxYValue: TLabeledEdit
    Left = 517
    Top = 74
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 15
    Text = '120'
  end
  object LabeledEditSection9MinYValue: TLabeledEdit
    Left = 422
    Top = 103
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 16
    Text = '90'
  end
  object LabeledEditSection9MaxYValue: TLabeledEdit
    Left = 517
    Top = 103
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 17
    Text = '110'
  end
  object LabeledEditSection10MinYValue: TLabeledEdit
    Left = 422
    Top = 132
    Width = 35
    Height = 25
    Hint = 'Input min value'
    EditLabel.Width = 43
    EditLabel.Height = 16
    EditLabel.Caption = 'min, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 18
    Text = '80'
  end
  object LabeledEditSection10MaxYValue: TLabeledEdit
    Left = 517
    Top = 132
    Width = 35
    Height = 25
    Hint = 'Input max value'
    EditLabel.Width = 46
    EditLabel.Height = 16
    EditLabel.Caption = 'max, '#176'C'
    EditLabel.Font.Charset = DEFAULT_CHARSET
    EditLabel.Font.Color = clWindowText
    EditLabel.Font.Height = -13
    EditLabel.Font.Name = 'Tahoma'
    EditLabel.Font.Style = []
    EditLabel.ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 19
    Text = '100'
  end
  object BitBtnOk: TBitBtn
    Left = 112
    Top = 170
    Width = 167
    Height = 48
    Caption = 'OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 20
    OnClick = BitBtnOkClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BitBtnCancel: TBitBtn
    Left = 296
    Top = 170
    Width = 167
    Height = 48
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 21
    OnClick = BitBtnCancelClick
    Kind = bkCancel
  end
end
