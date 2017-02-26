object FormTemperatureRanges: TFormTemperatureRanges
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Temperature Ranges'
  ClientHeight = 224
  ClientWidth = 523
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
    Width = 50
    Height = 14
    Caption = 'Section 1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection2: TLabel
    Left = 23
    Top = 48
    Width = 50
    Height = 14
    Caption = 'Section 2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection4: TLabel
    Left = 23
    Top = 104
    Width = 50
    Height = 14
    Caption = 'Section 4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection3: TLabel
    Left = 23
    Top = 76
    Width = 50
    Height = 14
    Caption = 'Section 3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection5: TLabel
    Left = 23
    Top = 133
    Width = 50
    Height = 14
    Caption = 'Section 5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection6: TLabel
    Left = 274
    Top = 20
    Width = 50
    Height = 14
    Caption = 'Section 6'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection7: TLabel
    Left = 274
    Top = 48
    Width = 50
    Height = 14
    Caption = 'Section 7'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection9: TLabel
    Left = 274
    Top = 104
    Width = 50
    Height = 14
    Caption = 'Section 9'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection8: TLabel
    Left = 274
    Top = 76
    Width = 50
    Height = 14
    Caption = 'Section 8'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelSection10: TLabel
    Left = 274
    Top = 133
    Width = 56
    Height = 14
    Caption = 'Section 10'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ButtonApply: TButton
    Left = 128
    Top = 178
    Width = 81
    Height = 25
    Caption = 'Apply'
    TabOrder = 20
    OnClick = ButtonApplyClick
  end
  object ButtonCancel: TButton
    Left = 304
    Top = 178
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 21
    OnClick = ButtonCancelClick
  end
  object LabeledEditSection1MinYValue: TLabeledEdit
    Left = 125
    Top = 16
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 0
    Text = '90'
  end
  object LabeledEditSection1MaxYValue: TLabeledEdit
    Left = 213
    Top = 16
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 1
    Text = '110'
  end
  object LabeledEditSection2MinYValue: TLabeledEdit
    Left = 125
    Top = 44
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 2
    Text = '100'
  end
  object LabeledEditSection2MaxYValue: TLabeledEdit
    Left = 213
    Top = 44
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 3
    Text = '120'
  end
  object LabeledEditSection3MinYValue: TLabeledEdit
    Left = 125
    Top = 72
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 4
    Text = '110'
  end
  object LabeledEditSection3MaxYValue: TLabeledEdit
    Left = 213
    Top = 72
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 5
    Text = '130'
  end
  object LabeledEditSection4MinYValue: TLabeledEdit
    Left = 125
    Top = 100
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 6
    Text = '120'
  end
  object LabeledEditSection4MaxYValue: TLabeledEdit
    Left = 213
    Top = 100
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 7
    Text = '140'
  end
  object LabeledEditSection5MinYValue: TLabeledEdit
    Left = 125
    Top = 128
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 8
    Text = '130'
  end
  object LabeledEditSection5MaxYValue: TLabeledEdit
    Left = 213
    Top = 128
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 9
    Text = '150'
  end
  object LabeledEditSection6MinYValue: TLabeledEdit
    Left = 376
    Top = 16
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 10
    Text = '120'
  end
  object LabeledEditSection6MaxYValue: TLabeledEdit
    Left = 464
    Top = 16
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 11
    Text = '140'
  end
  object LabeledEditSection7MinYValue: TLabeledEdit
    Left = 376
    Top = 44
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 12
    Text = '110'
  end
  object LabeledEditSection7MaxYValue: TLabeledEdit
    Left = 464
    Top = 44
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 13
    Text = '130'
  end
  object LabeledEditSection8MinYValue: TLabeledEdit
    Left = 376
    Top = 72
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 14
    Text = '100'
  end
  object LabeledEditSection8MaxYValue: TLabeledEdit
    Left = 464
    Top = 72
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 15
    Text = '120'
  end
  object LabeledEditSection9MinYValue: TLabeledEdit
    Left = 376
    Top = 100
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 16
    Text = '90'
  end
  object LabeledEditSection9MaxYValue: TLabeledEdit
    Left = 464
    Top = 100
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 17
    Text = '110'
  end
  object LabeledEditSection10MinYValue: TLabeledEdit
    Left = 376
    Top = 128
    Width = 28
    Height = 22
    EditLabel.Width = 35
    EditLabel.Height = 13
    EditLabel.Caption = 'min, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 18
    Text = '80'
  end
  object LabeledEditSection10MaxYValue: TLabeledEdit
    Left = 464
    Top = 128
    Width = 28
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'max, '#176'C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    TabOrder = 19
    Text = '100'
  end
end
