unit CIncomingComPortMessage;

interface

uses Classes, CBasicComPortMessage;

const
  MAX_BYTES_COUNT = 25; //ћаксимальное количество байт в сообщении
  TIMEOUT_END_PACKET = 100;

type
  TypeIncomingMessageState = (imsNone,
                              imsWaitEndPacket, //часть данных получена, ожидаетс€ продолжение
                              imsRecieved); //все данные получены (фактически или по тайм-ауту)

  TypeIncomingMessageError = (imeNone,
                              imeCRC,
                              imeNoSendingMessage,
                              imeWrongCmdOrDeviceId,
                              imeBufferOverflow,
                              imeTimeoutEndPacket);

type
  TMIncomingComportMessage = class (TMBasicComPortMessage)

  private
    FState     : TypeIncomingMessageState;
    FError     : TypeIncomingMessageError;

    FMessageLength : Byte; //ƒлина вход€щего сообщени€ в байтах (включа€ CRC, но без DeviceId и CommandId

    FRecievedPartTime : TDateTime; //¬рем€ получени€ последней пачки сообщений
    FRecievedTime     : TDateTime; //¬рем€ получени€ сообщени€ (считаетс€ момент, когда пришла перва€ пачка)

    FIncomingByteIndex : integer;
    FMaxBytesCount     : integer;

    function CheckCRC() : boolean;

  public
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


implementation

uses SysUtils, DateUtils;

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

      if FIncomingByteIndex >= FMaxBytesCount    //Ќа случай переполнени€ буфера
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
              else  //$06 - echo ответ
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

        for i := 3 to len - 1 do
          AMessageBytes[i] := FDataBytes[i-3];

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
  GenerateMessage(MessageBytes);

  len := Length(MessageBytes);

  SetLength(MessageBytes, len - 2);

  getCRC16(@MessageBytes[0], len - 2, Hi, Lo);

  Result := (Hi = FCRCHi) and (Lo = FCRCLo);
end;

end.
