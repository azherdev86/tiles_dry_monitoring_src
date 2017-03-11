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
  object ButtonGenerateBase: TButton
    Left = 22
    Top = 23
    Width = 209
    Height = 23
    Caption = 'GENERATE BASE'
    TabOrder = 0
    OnClick = ButtonGenerateBaseClick
  end
  object ButtonResetBase: TButton
    Left = 22
    Top = 46
    Width = 209
    Height = 24
    Caption = 'RESET BASE'
    TabOrder = 1
    OnClick = ButtonResetBaseClick
  end
  object ButtonStartStopTimers: TButton
    Left = 22
    Top = 87
    Width = 209
    Height = 24
    Caption = 'STOP/START TIMERS'
    TabOrder = 2
    OnClick = ButtonStartStopTimersClick
  end
  object ButtonSignalState: TButton
    Left = 22
    Top = 173
    Width = 209
    Height = 24
    Caption = 'SIGNAL STATE'
    TabOrder = 3
  end
  object ButtonEnableSignal: TButton
    Left = 22
    Top = 197
    Width = 209
    Height = 24
    Caption = 'ENABLE SIGNAL'
    TabOrder = 4
    OnClick = ButtonEnableSignalClick
  end
  object ButtonDisableSignal: TButton
    Left = 22
    Top = 221
    Width = 209
    Height = 24
    Caption = 'DISABLE SIGNAL'
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
    Font.Name = 'Tahoma'
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
    Left = 22
    Top = 112
    Width = 209
    Height = 24
    Caption = 'GENERATE BOX MESSAGE'
    TabOrder = 8
    OnClick = ButtonGenerateBoxMessageClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 80
    Top = 269
  end
end
