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
    function PrepareText(AText : string) : string;
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
var
  text : string;
begin
  Application.CreateForm(TFormUserDigitalKeyboard, FormUserDigitalKeyboard);
  try
    text := (Sender as TLabeledEdit).Text;
    text := PrepareText(text);
    
    FormUserDigitalKeyboard.EditResult.Text := text;

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

function TFormTerminal.PrepareText(AText : string) : string;
var
  is_zero : boolean;
begin
  Result := AText;

  is_zero := True;

  if Length(AText) < 1 
    then Exit;        

  while is_zero do
    begin
      if (AText[1] = '0') and (Length(AText) > 1)
        then AText := StringReplace(AText, '0', '', [])
        else is_zero := False;      
    end;  

  Result := AText;
end;


end.
