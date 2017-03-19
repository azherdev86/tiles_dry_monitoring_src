object FormExportToCSV: TFormExportToCSV
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Export data to CSV'
  ClientHeight = 122
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelSince: TLabel
    Left = 37
    Top = 12
    Width = 40
    Height = 16
    Caption = 'Since:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelTo: TLabel
    Left = 58
    Top = 46
    Width = 19
    Height = 16
    Caption = 'To:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Gauge: TGauge
    Left = 24
    Top = 80
    Width = 609
    Height = 25
    Progress = 0
    Visible = False
  end
  object DatePickerSince: TDateTimePicker
    Left = 85
    Top = 8
    Width = 101
    Height = 26
    Date = 42785.011825775470000000
    Time = 42785.011825775470000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object DatePickerTo: TDateTimePicker
    Left = 85
    Top = 42
    Width = 102
    Height = 26
    Date = 42785.011825775470000000
    Time = 42785.011825775470000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object LabeledEditSinceHours: TLabeledEdit
    Left = 195
    Top = 8
    Width = 26
    Height = 27
    Hint = 'Input since hours'
    EditLabel.Width = 108
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEditSinceHours'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '01'
    OnChange = LabeledEditSinceHoursChange
  end
  object LabeledEditSinceMinutes: TLabeledEdit
    Left = 232
    Top = 8
    Width = 26
    Height = 27
    Hint = 'Input since minutes'
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEditSinceMinutes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Text = '12'
    OnChange = LabeledEditSinceMinutesChange
  end
  object LabeledEditToHours: TLabeledEdit
    Left = 195
    Top = 42
    Width = 26
    Height = 27
    Hint = 'Input to hours'
    EditLabel.Width = 95
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEditToHours'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = '02'
    OnChange = LabeledEditToHoursChange
  end
  object LabeledEditToMinutes: TLabeledEdit
    Left = 232
    Top = 42
    Width = 26
    Height = 27
    Hint = 'Input since minutes'
    EditLabel.Width = 104
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEditToMinutes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = '12'
    OnChange = LabeledEditToMinutesChange
  end
  object BitBtnOk: TBitBtn
    Left = 277
    Top = 16
    Width = 167
    Height = 48
    Caption = 'Export'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
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
    Left = 466
    Top = 16
    Width = 167
    Height = 48
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = BitBtnCancelClick
    Kind = bkCancel
  end
  object SaveDialog: TSaveDialog
    Filter = 'CSV (Comma separated values)|*.csv'
    Top = 65535
  end
end
