unit uTDurationProcess;

interface

uses
  Windows, Classes, StdCtrls;

type
  TDurationProcess = class
  protected
    _Frequency: Int64;
    _OnBegin:   Int64;
    _OnEnd:     Int64;
  public
    procedure dpBegin;
    function  dpEnd()          : Double;
  end;

var
  _Dur: TDurationProcess;

procedure dpBegin;
function dpEnd(): Double;

implementation

procedure TDurationProcess.dpBegin;
begin
  QueryPerformanceFrequency(_Frequency);
	QueryPerformanceCounter(_OnBegin);
end;

function TDurationProcess.dpEnd(): Double;
begin
  QueryPerformanceCounter(_OnEnd);
  if _Frequency = 0
    then Result := 0
    else Result := (_OnEnd-_OnBegin)/_Frequency;
end;

procedure dpBegin;
begin
  _Dur.dpBegin;
end;

function dpEnd(): Double;
begin
  result:=_Dur.dpEnd
end;

initialization
  _Dur:= TDurationProcess.Create;

finalization
  _Dur.Free;

end.
