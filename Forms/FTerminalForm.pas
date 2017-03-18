unit FTerminalForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TFormTerminal = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SetDigitalKeyboardForInputs;
    procedure OnInputClick(Sender : TObject);
  public
    { Public declarations }
  end;

var
  FormTerminal: TFormTerminal;

implementation

uses FUserDigitalKeyboard, LApplicationGlobals;

{$R *.dfm}

procedure TFormTerminal.FormCreate(Sender: TObject);
begin
  SetDigitalKeyboardForInputs;
end;

procedure TFormTerminal.SetDigitalKeyboardForInputs;
var
  i, count : integer;

  Component : TComponent;
begin
  count := ComponentCount;

  for i := 0 to count - 1 do
    begin
      Component := Components[i];

      if not (Component is TLabeledEdit)
        then Continue;

      (Component as TLabeledEdit).OnClick := OnInputClick;
    end;

end;

procedure TFormTerminal.OnInputClick(Sender : TObject);
begin
  Application.CreateForm(TFormUserDigitalKeyboard, FormUserDigitalKeyboard);
  try
    FormUserDigitalKeyboard.EditResult.Text := (Sender as TLabeledEdit).Text;
    if (Sender as TLabeledEdit).Hint <> ''
      then FormUserDigitalKeyboard.Caption := (Sender as TLabeledEdit).Hint;

    FormUserDigitalKeyboard.ShowModal;

    if FormUserDigitalKeyboard.ResultOperation = roEnter
      then (Sender as TLabeledEdit).Text := FloatToStr(FormUserDigitalKeyboard.ResultValue, ApplicationFormatSettings);

    (Sender as TLabeledEdit).SelectAll;
  finally
    FormUserDigitalKeyboard.Free;
  end;
end;


end.
