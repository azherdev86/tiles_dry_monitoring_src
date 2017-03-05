object FormEventLogs: TFormEventLogs
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Event history'
  ClientHeight = 400
  ClientWidth = 1309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object DBGrid: TDBGrid
    Left = 0
    Top = 8
    Width = 1305
    Height = 325
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Arial'
    Font.Style = []
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Arial'
    TitleFont.Style = []
  end
  object ButtonRefresh: TButton
    Left = 529
    Top = 354
    Width = 81
    Height = 25
    Caption = 'Refresh'
    TabOrder = 1
    OnClick = ButtonRefreshClick
  end
  object ButtonCancel: TButton
    Left = 682
    Top = 354
    Width = 81
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
  object DataSource: TDataSource
  end
end
