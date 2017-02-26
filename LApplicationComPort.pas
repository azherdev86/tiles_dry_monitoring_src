unit LApplicationComPort;

interface

procedure ComPortRxChar(Sender: TObject; Count: Integer);
procedure ComPortSendData();

type TDynamicByteArray = array of Byte;

var
  DataArray : TDynamicByteArray;

implementation

uses LApplicationGlobals, Dialogs;


procedure InitByteArray(bArray : TDynamicByteArray);
begin
  SetLength(bArray, 0);
end;

procedure ComPortRxChar(Sender: TObject; Count: Integer);
begin
//  ApplicationComPort.Read()

//  App.Read(DataRead, Count);
//
//  for i := 0 to Count - 1 do
//    Memo.Lines.Add(IntToHex(DataRead[i], 2));


//  ComPort.ReadStr(Str, Count);



  //Memo.Text := Memo.Text +   IntToHex(StrToInt(Str), 2);


end;

procedure ComPortSendData();
begin
//  Data[0] := $0A;
//  Data[1] := $03;
//  Data[2] := $00;
//  Data[3] := $00;
//  Data[4] := $00;
//  Data[5] := $0A;
//  Data[6] := $C4;
//  Data[7] := $B6;

//  getCRC16(@Data, 6, Hi, Lo);
//
//  Data[6] := Hi;
//  Data[7] := Lo;
//
//  Memo.Lines.Add(IntToHex(Hi, 0) + ' ' + IntToHex(Lo, 0));



  //Memo.Lines.Add(IntToStr(ComPort.Write(Data, 8)));  //Length(Data)

end;

end.
