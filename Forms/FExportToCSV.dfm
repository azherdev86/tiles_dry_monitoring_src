object FormExportToCSV: TFormExportToCSV
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Export data to CSV'
  ClientHeight = 87
  ClientWidth = 614
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
    Left = 271
    Top = 12
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
    Left = 51
    Top = 48
    Width = 513
    Height = 25
    Progress = 0
    Visible = False
  end
  object ButtonExport: TButton
    Left = 481
    Top = 8
    Width = 83
    Height = 25
    Caption = 'Export'
    TabOrder = 0
    OnClick = ButtonExportClick
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
    TabOrder = 1
  end
  object DatePickerTo: TDateTimePicker
    Left = 297
    Top = 8
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
    TabOrder = 2
  end
  object LabeledEditSinceHours: TLabeledEdit
    Left = 192
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
    TabOrder = 3
    Text = '01'
    OnChange = LabeledEditSinceHoursChange
  end
  object LabeledEditSinceMinutes: TLabeledEdit
    Left = 229
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
    TabOrder = 4
    Text = '12'
    OnChange = LabeledEditSinceMinutesChange
  end
  object LabeledEditToHours: TLabeledEdit
    Left = 407
    Top = 8
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
    TabOrder = 5
    Text = '02'
    OnChange = LabeledEditToHoursChange
  end
  object LabeledEditToMinutes: TLabeledEdit
    Left = 444
    Top = 8
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
    TabOrder = 6
    Text = '12'
    OnChange = LabeledEditToMinutesChange
  end
  object SaveDialog: TSaveDialog
    Filter = 'CSV (Comma separated values)|*.csv'
    Top = 65535
  end
end
