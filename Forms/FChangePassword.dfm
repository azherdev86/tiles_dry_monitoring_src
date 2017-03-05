object FormChangePassword: TFormChangePassword
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Change admin password'
  ClientHeight = 162
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonCancel: TButton
    Left = 187
    Top = 125
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = ButtonCancelClick
  end
  object ButtonOk: TButton
    Left = 103
    Top = 125
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = ButtonOkClick
  end
  object LabeledEditCurrentPassword: TLabeledEdit
    Left = 134
    Top = 10
    Width = 209
    Height = 21
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = 'Input current password:'
    LabelPosition = lpLeft
    PasswordChar = '*'
    TabOrder = 0
    OnKeyDown = LabeledEditCurrentPasswordKeyDown
  end
  object LabeledEditNewPassword: TLabeledEdit
    Left = 134
    Top = 38
    Width = 209
    Height = 21
    EditLabel.Width = 102
    EditLabel.Height = 13
    EditLabel.Caption = 'Input new password:'
    LabelPosition = lpLeft
    PasswordChar = '*'
    TabOrder = 1
    OnKeyDown = LabeledEditNewPasswordKeyDown
  end
  object LabeledEditNewPasswordConfirm: TLabeledEdit
    Left = 134
    Top = 66
    Width = 209
    Height = 21
    EditLabel.Width = 113
    EditLabel.Height = 13
    EditLabel.Caption = 'Confirm new password:'
    LabelPosition = lpLeft
    PasswordChar = '*'
    TabOrder = 2
    OnKeyDown = LabeledEditNewPasswordConfirmKeyDown
  end
  object CheckBoxDisplayInputCharacters: TCheckBox
    Left = 119
    Top = 93
    Width = 132
    Height = 23
    Caption = 'Display input characters'
    TabOrder = 5
    OnClick = CheckBoxDisplayInputCharactersClick
    OnKeyDown = CheckBoxDisplayInputCharactersKeyDown
  end
  object ActionList: TActionList
    Left = 8
    Top = 96
    object ActionLabeledEditKeyDown: TAction
      Caption = 'ActionLabeledEditKeyDown'
    end
  end
end
