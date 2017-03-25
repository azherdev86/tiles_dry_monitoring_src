object FormChangePassword: TFormChangePassword
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Change admin password'
  ClientHeight = 183
  ClientWidth = 408
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
  object LabeledEditCurrentPassword: TLabeledEdit
    Left = 134
    Top = 10
    Width = 256
    Height = 22
    Hint = 'Input current password'
    EditLabel.Width = 117
    EditLabel.Height = 13
    EditLabel.Caption = 'Input current password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 0
    OnKeyDown = LabeledEditCurrentPasswordKeyDown
  end
  object LabeledEditNewPassword: TLabeledEdit
    Left = 134
    Top = 38
    Width = 256
    Height = 22
    Hint = 'Input new password'
    EditLabel.Width = 102
    EditLabel.Height = 13
    EditLabel.Caption = 'Input new password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 1
    OnKeyDown = LabeledEditNewPasswordKeyDown
  end
  object LabeledEditNewPasswordConfirm: TLabeledEdit
    Left = 134
    Top = 66
    Width = 256
    Height = 22
    Hint = 'Confirm new password'
    EditLabel.Width = 113
    EditLabel.Height = 13
    EditLabel.Caption = 'Confirm new password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    LabelPosition = lpLeft
    ParentFont = False
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = CheckBoxDisplayInputCharactersClick
    OnKeyDown = CheckBoxDisplayInputCharactersKeyDown
  end
  object BitBtnOk: TBitBtn
    Left = 32
    Top = 122
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
    TabOrder = 4
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
    Left = 216
    Top = 122
    Width = 167
    Height = 48
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = BitBtnCancelClick
    Kind = bkCancel
  end
  object ActionList: TActionList
    Left = 8
    Top = 96
    object ActionLabeledEditKeyDown: TAction
      Caption = 'ActionLabeledEditKeyDown'
    end
  end
end
