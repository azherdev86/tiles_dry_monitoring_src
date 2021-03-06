unit CIncomingComPortMessage;

interface

uses Classes, CBasicComPortMessage;

const
  MAX_BYTES_COUNT = 25; //������������ ���������� ���� � ���������
  TIMEOUT_END_PACKET = 100;

type
  TypeIncomingMessageState = (imsNone,
                              imsWaitEndPacket, //����� ������ ��������, ��������� �����������
                              imsRecieved); //��� ������ �������� (���������� ��� �� ����-����)

  TypeIncomingMessageError = (imeNone,
                              imeCRC,
                              imeNoSendingMessage,
                              imeSendingMessage, //������ � ������������ ���������
                              imeWrongDeviceId,
                              imeBufferOverflow,
                              imeTimeoutEndPacket);

type
  TMIncomingComportMessage = class (TMBasicComPortMessage)

  private
    FState     : TypeIncomingMessageState;
    FError     : TypeIncomingMessageError;

    FMessageLength : Byte; //����� ��������� ��������� � ������ (������� CRC, �� ��� DeviceId � CommandId

    FRecievedPartTime : TDateTime; //����� ��������� ��������� ����� ���������
    FRecievedTime     : TDateTime; //����� ��������� ��������� (��������� ������, ����� ������ ������ �����)

    FIncomingByteIndex : integer;
    FMaxBytesCount     : integer;

    function CheckCRC() : boolean;

  public
    procedure SaveToDataBase(); override;
    function LoadFromBuffer(ABuffer : TDynamicByteArray) : boolean;
    function IsError() : boolean;

    procedure GenerateMessage(var AMessageBytes : TDynamicByteArray); override;
  protected
    procedure Reset(); override;

  public
    property State : TypeIncomingMessageState read FState write FState;
    property Error : TypeIncomingMessageError read FError write FError;
    property IncomingByteIndex : integer read FIncomingByteIndex;

    property RecievedTime : TDateTime read FRecievedTime write FRecievedTime;
  end;

  function IncomingMessageStateToStr(AMessageState: TypeIncomingMessageState): string;
  function StrToIncomingMessageState(AStr: string): TypeIncomingMessageState;

  function IncomingMessageErrorToStr(AMessageError: TypeIncomingMessageError): string;
  function StrToIncomingMessageError(AStr: string): TypeIncomingMessageError;

implementation

uses SysUtils, DateUtils, Dialogs, LApplicationGlobals, CEventLog;


function IncomingMessageStateToStr(AMessageState: TypeIncomingMessageState): string;
const
  MessageStateStrings: array[TypeIncomingMessageState] of string =
 ('None', 'WaitEndPacket', 'Recieved');
begin
  Result := MessageStateStrings[AMessageState];
end;

function StrToIncomingMessageState(AStr: string): TypeIncomingMessageState;
var
  I: TypeIncomingMessageState;
begin
  I := Low(TypeIncomingMessageState);
  while (I <= High(TypeIncomingMessageState)) do
    begin
      if UpperCase(AStr) = UpperCase(IncomingMessageStateToStr(TypeIncomingMessageState(I)))
        then Break;
      I := Succ(I);
    end;

  Result := I;
end;

function IncomingMessageErrorToStr(AMessageError: TypeIncomingMessageError): string;
const
  MessageErrorStrings: array[TypeIncomingMessageError] of string =
 ('None', 'CRC', 'NoSendingMessage', 'SendingMessage',
  'WrongDeviceId', 'BufferOverflow', 'TimeoutEndPacket');
begin
  Result := MessageErrorStrings[AMessageError];
end;

function StrToIncomingMessageError(AStr: string): TypeIncomingMessageError;
var
  I: TypeIncomingMessageError;
begin
  I := Low(TypeIncomingMessageError);
  while (I <= High(TypeIncomingMessageError)) do
    begin
      if UpperCase(AStr) = UpperCase(IncomingMessageErrorToStr(TypeIncomingMessageError(I)))
        then Break;
      I := Succ(I);
    end;

  Result := I;
end;



function TMIncomingComportMessage.IsError() : boolean;
begin
  Result := not (FError = imeNone);
end;

function TMIncomingComportMessage.LoadFromBuffer(ABuffer : TDynamicByteArray) : boolean;
var
  count, i,
  data_index,
  ms : integer;
begin
  count := Length(ABuffer);

  case FState of
    imsNone:
      begin
        FState := imsWaitEndPacket;
        FRecievedTime := Now;
        FRecievedPartTime := FRecievedTime;
      end;
    imsWaitEndPacket:
      begin
        ms := MilliSecondsBetween(FRecievedPartTime, Now);
        FRecievedPartTime := Now;

        if TIMEOUT_END_PACKET < abs(ms)
          then
            begin
              Result := TRUE;
              FError := imeTimeoutEndPacket;
              Exit;
            end;
      end;
    imsRecieved:
      begin
        Result := TRUE;
        Exit;
      end;
  end;

  for i := 0 to count - 1 do
    begin
      if FState = imsRecieved
        then break;

      if FIncomingByteIndex >= FMaxBytesCount    //�� ������ ������������ ������
        then
          begin
            FError := imeBufferOverflow;
            Break;
          end;

      case FIncomingByteIndex of
        0 : FDeviceId      := ABuffer[i];
        1 : FCommandId     := ABuffer[i];
        else
          begin
            if (FCommandId = $04) or (FCommandId = $03)
              then // $03 $04
                begin
                  if FIncomingByteIndex = 2
                    then
                      begin
                        FMessageLength := ABuffer[i];
                        SetLength(FDataBytes, FMessageLength);
                      end
                    else
                      begin
                        data_index := FIncomingByteIndex - 3;

                        if data_index < FMessageLength
                          then FDataBytes[data_index] := ABuffer[i];

                        if (data_index) = FMessageLength
                          then FCRCHi := ABuffer[i];

                        if (data_index) = FMessageLength + 1
                          then
                            begin
                              FCRCLo := ABuffer[i];
                              FState := imsRecieved;
                            end;
                      end;
                end
              else  //$06 - echo �����
                begin
                  if FIncomingByteIndex = 2
                    then FMSBRegisterAddr := ABuffer[i];

                  if FIncomingByteIndex = 3
                    then
                      begin
                        FLSBRegisterAddr := ABuffer[i];
                        FMessageLength := 2;
                        SetLength(FDataBytes, FMessageLength);
                      end;

                  if FIncomingByteIndex > 3
                    then
                      begin
                        data_index := FIncomingByteIndex - 4;

                        if data_index < FMessageLength
                          then FDataBytes[data_index] := ABuffer[i];

                        if (data_index) = FMessageLength
                          then FCRCHi := ABuffer[i];

                        if (data_index) = FMessageLength + 1
                          then
                            begin
                              FCRCLo := ABuffer[i];
                              FState := imsRecieved;
                            end;
                      end;
                end;
          end;
      end;

      INC(FIncomingByteIndex);
    end;

  if FState = imsRecieved
    then
      begin
        if not (FError = imeBufferOverflow)
          then
            begin
              if not CheckCRC
                then
                  begin
                    FError := imeCRC;
                    FState := imsWaitEndPacket;
                  end;
            end;
      end;

  Result := True;
end;


procedure TMIncomingComportMessage.SaveToDataBase();
begin
  if not Assigned(FTableRecord)
    then Exit;

  FTableRecord.ClearRecordValues;

  FTableRecord.FieldByName['MessageType'].Value             := 'in';
  FTableRecord.FieldByName['MessageRecievedTime'].Value     := FRecievedTime;
  FTableRecord.FieldByName['MessageRecievedPartTime'].Value := FRecievedPartTime;
  FTableRecord.FieldByName['MessageState'].Value            := IncomingMessageStateToStr(FState);
  FTableRecord.FieldByName['MessageError'].Value            := IncomingMessageErrorToStr(FError);
  FTableRecord.FieldByName['MessageBytes'].Value            := GenerateMessageString;
  FTableRecord.FieldByName['MessageUid'].Value              := FMessageUid;

  FTableRecord.AddRecord;
end;


procedure TMIncomingComportMessage.GenerateMessage(var AMessageBytes : TDynamicByteArray);
var
  len,
  i : integer;
begin
  case FCommandId of
    $03, $04 :
      begin
        len := Length(FDataBytes) + 5;

        SetLength(AMessageBytes, len);

        AMessageBytes[0] := FDeviceId;
        AMessageBytes[1] := FCommandId;
        AMessageBytes[2] := FMessageLength;

//        try
          for i := 3 to len - 1 do
            AMessageBytes[i] := FDataBytes[i-3];
//        except
//          ApplicationEventLog.WriteLog(elProgramExcep,
//                                       'TMIncomingComPortMessage.GenerateMessage. ' +
//                                       'Len = ' + IntToStr(len) + '; FDataBytes = ' +
//                                        IntToStr(Length(FDataBytes)))
//        end;

        AMessageBytes[len - 2] := FCRCHi;
        AMessageBytes[len - 1] := FCRCLo;
      end;
    $06      :
      begin
        len := Length(FDataBytes) + 6;

        SetLength(AMessageBytes, len);

        AMessageBytes[0] := FDeviceId;
        AMessageBytes[1] := FCommandId;
        AMessageBytes[2] := FMSBRegisterAddr;
        AMessageBytes[3] := FLSBRegisterAddr;
        AMessageBytes[4] := FDataBytes[0];
        AMessageBytes[5] := FDataBytes[1];
        AMessageBytes[6] := FCRCHi;
        AMessageBytes[7] := FCRCLo;
      end;
  end;


end;


procedure TMIncomingComportMessage.Reset();
begin
  inherited;

  FState := imsNone;
  FError := imeNone;

  FIncomingByteIndex := 0;

  FMessageLength := $00;

  FRecievedTime     := 0;
  FRecievedPartTime := 0;

  FMaxBytesCount := MAX_BYTES_COUNT;
end;


function TMIncomingComportMessage.CheckCRC() : boolean;
var
  Hi, Lo : Byte;

  len : integer;

  MessageBytes : TDynamicByteArray;
begin
  Result := False;

  GenerateMessage(MessageBytes);

  len := Length(MessageBytes);

  if (len - 2) > 0
    then
      begin
        SetLength(MessageBytes, len - 2);

        getCRC16(@MessageBytes[0], len - 2, Hi, Lo);
        Result := (Hi = FCRCHi) and (Lo = FCRCLo);
      end;
end;

end.
