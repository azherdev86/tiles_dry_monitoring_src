unit FChangePassword;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, FTerminalForm;

type
  TFormChangePassword = class(TFormTerminal)
    ButtonCancel: TButton;
    ButtonOk: TButton;
    LabeledEditCurrentPassword: TLabeledEdit;
    LabeledEditNewPassword: TLabeledEdit;
    LabeledEditNewPasswordConfirm: TLabeledEdit;
    ActionList: TActionList;
    ActionLabeledEditKeyDown: TAction;
    CheckBoxDisplayInputCharacters: TCheckBox;
    procedure LabeledEditCurrentPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditNewPasswordKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditNewPasswordConfirmKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure CheckBoxDisplayInputCharactersClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure CheckBoxDisplayInputCharactersKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    procedure LabeledEditKeyDown(var Key : Word);
  public
    { Public declarations }
  end;

var
  FormChangePassword: TFormChangePassword;

implementation

uses LApplicationGlobals, LHash, CEventLog;

{$R *.dfm}

procedure TFormChangePassword.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormChangePassword.ButtonOkClick(Sender: TObject);
var
  CurrentPass,
  NewPass,
  ConfirmPass : string;
  Hash : string;
begin
  CurrentPass := LabeledEditCurrentPassword.Text;
  NewPass     := LabeledEditNewPassword.Text;
  ConfirmPass := LabeledEditNewPasswordConfirm.Text;

  if CurrentPass = ''
    then
      begin
        ShowMessage('Current password required');
        LabeledEditCurrentPassword.SetFocus;
        Exit;
      end;

  if NewPass = ''
    then
      begin
        ShowMessage('New password required');
        LabeledEditNewPassword.SetFocus;
        Exit;
      end;

  if ConfirmPass = ''
    then
      begin
        ShowMessage('New password confirm required');
        LabeledEditNewPasswordConfirm.SetFocus;
        Exit;
      end;


  Hash := ApplicationProgramSettings.UserSettings.PasswordHash;

  if MD5DigestToStr(MD5String(CurrentPass)) <> Hash
    then
      begin
        ShowMessage('Wrong current password');
        LabeledEditCurrentPassword.SetFocus;
        Exit;
      end;

  if NewPass <> ConfirmPass
    then
      begin
        ShowMessage('New password and confirmation do not match');
        LabeledEditNewPassword.SetFocus;
        Exit;
      end;


  Hash := MD5DigestToStr(MD5String(LabeledEditNewPassword.Text));

  ApplicationProgramSettings.UserSettings.PasswordHash := Hash;

  ApplicationProgramSettings.SaveToInifile;

  ApplicationEventLog.WriteLog(elChangePass);

  Close;
end;

procedure TFormChangePassword.CheckBoxDisplayInputCharactersClick(
  Sender: TObject);
begin
  case CheckBoxDisplayInputCharacters.Checked of
    False  :
      begin
        LabeledEditCurrentPassword.PasswordChar    := '*';
        LabeledEditNewPassword.PasswordChar        := '*';
        LabeledEditNewPasswordConfirm.PasswordChar := '*';
      end;

    True :
      begin
        LabeledEditCurrentPassword.PasswordChar    := #0;
        LabeledEditNewPassword.PasswordChar        := #0;
        LabeledEditNewPasswordConfirm.PasswordChar := #0;
      end;
  end;
end;

procedure TFormChangePassword.CheckBoxDisplayInputCharactersKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  LabeledEditKeyDown(Key);
end;

procedure TFormChangePassword.LabeledEditCurrentPasswordKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  LabeledEditKeyDown(Key);
end;

procedure TFormChangePassword.LabeledEditKeyDown(var Key: Word);
begin
  case Key of
    VK_RETURN: ButtonOk.Click;
    VK_ESCAPE:
      begin
//        PasswordMode := pmNone;
        Close;
      end;
  end;
end;

procedure TFormChangePassword.LabeledEditNewPasswordConfirmKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  LabeledEditKeyDown(Key);
end;

procedure TFormChangePassword.LabeledEditNewPasswordKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  LabeledEditKeyDown(Key);
end;

end.
