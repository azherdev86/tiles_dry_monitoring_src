object FormInputPassword: TFormInputPassword
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Password required'
  ClientHeight = 84
  ClientWidth = 227
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
  object LabeledEditPassword: TLabeledEdit
    Left = 8
    Top = 24
    Width = 209
    Height = 21
    EditLabel.Width = 167
    EditLabel.Height = 13
    EditLabel.Caption = 'Input admin password to continue:'
    PasswordChar = '*'
    TabOrder = 0
    OnKeyDown = LabeledEditPasswordKeyDown
  end
  object ButtonOk: TButton
    Left = 32
    Top = 51
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = ButtonOkClick
  end
  object ButtonCancel: TButton
    Left = 120
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = ButtonCancelClick
  end
end
