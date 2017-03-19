object FormDebugPanel: TFormDebugPanel
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'FormDebugPanel'
  ClientHeight = 406
  ClientWidth = 827
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    827
    406)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 248
    Width = 195
    Height = 16
    Caption = 'Warning. Could delete all data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ButtonEmulateDataInBase: TButton
    Left = 38
    Top = 307
    Width = 209
    Height = 23
    Caption = 'EMULATE DATA IN BASE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = ButtonEmulateDataInBaseClick
  end
  object ButtonResetBase: TButton
    Left = 38
    Top = 282
    Width = 209
    Height = 24
    Caption = 'RESET BASE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonResetBaseClick
  end
  object ButtonStartStopTimers: TButton
    Left = 38
    Top = 87
    Width = 209
    Height = 24
    Caption = 'ON/OFF SEND COMPORT MESSAGES'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = ButtonStartStopTimersClick
  end
  object ButtonSignalState: TButton
    Left = 38
    Top = 133
    Width = 209
    Height = 24
    Caption = 'GET SIGNAL STATE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object ButtonEnableSignal: TButton
    Left = 38
    Top = 157
    Width = 209
    Height = 24
    Caption = 'ENABLE SIGNAL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = ButtonEnableSignalClick
  end
  object ButtonDisableSignal: TButton
    Left = 38
    Top = 181
    Width = 209
    Height = 24
    Caption = 'DISABLE SIGNAL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = ButtonDisableSignalClick
  end
  object MemoInfo: TMemo
    Left = 284
    Top = 8
    Width = 506
    Height = 338
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object MemoLogs: TMemo
    Left = 284
    Top = 352
    Width = 506
    Height = 43
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object ButtonGenerateBoxMessage: TButton
    Left = 38
    Top = 63
    Width = 209
    Height = 24
    Caption = 'GENERATE BOX MESSAGE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    OnClick = ButtonGenerateBoxMessageClick
  end
  object ButtonKeyboard: TButton
    Left = 40
    Top = 218
    Width = 209
    Height = 24
    Caption = 'DISABLE SIGNAL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnClick = ButtonKeyboardClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 16
    Top = 349
  end
end
