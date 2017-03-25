unit CBasicComPortMessage;

interface

uses Classes, CTableRecords;

const
  MAX_MESSAGES_COUNT = 100;

type
  TypeMessagePriority = (mpNone,
                         mpNormal,
                         mpHigh);

type
  TDynamicByteArray = array of Byte;


type
  TMBasicComPortMessage = class
    constructor Create();
    destructor Destroy(); override;

  private
    function GenerateUid() : string;

  protected
    FPriority : TypeMessagePriority;

    FDataBytes : TDynamicByteArray;

    FDeviceId       : Byte;
    FCommandId      : Byte;
    FMSBRegisterAddr : Byte;
    FLSBRegisterAddr : Byte;

    FCRCHi     : Byte;
    FCRCLo     : Byte;

    FMessageUid : string;

  protected
    FTableRecord : TMTableRecord;

    procedure Reset(); virtual;

  public
//    DebugDeviceId : Byte; //Ќа период отладки, чтобы можно эмулировать работу 20 коробок на 1-ом устройстве

    procedure GenerateMessage(var AMessageBytes : TDynamicByteArray); virtual; abstract;
    procedure SaveToDataBase(); virtual; abstract;
    procedure LoadDataBytes(ADataBytes : TDynamicByteArray);
    procedure SaveDataBytes(var ADataBytes : TDynamicByteArray);

    function GenerateMessageString() : string;

    procedure Clear();

  public
    property Priority        : TypeMessagePriority  read FPriority        write FPriority;
    property DeviceId        : Byte                 read FDeviceId        write FDeviceId;
    property CommandId       : Byte                 read FCommandId       write FCommandId;
    property MSBRegisterAddr : Byte                 read FMSBRegisterAddr write FMSBRegisterAddr;
    property LSBRegisterAddr : Byte                 read FLSBRegisterAddr write FLSBRegisterAddr;
    property MessageUid      : string               read FMessageUid;
  end;

procedure getCRC16(p : pointer; len : word; var Hi : byte; var Lo : byte);


implementation
uses SysUtils;

type
  tByteArray = Array[0..63999] of byte;

const
  CRChi : array[0..255] of byte =
 ($00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $01,$C0,$80,$41,$00,$C1,$81,$40,$00,$C1,$81,$40,$01,$C0,$80,$41,
  $00,$C1,$81,$40,$01,$C0,$80,$41,$01,$C0,$80,$41,$00,$C1,$81,$40 );

  CRClo : array[0..255] of byte =
 ($00,$C0,$C1,$01,$C3,$03,$02,$C2,$C6,$06,$07,$C7,$05,$C5,$C4,$04,
  $CC,$0C,$0D,$CD,$0F,$CF,$CE,$0E,$0A,$CA,$CB,$0B,$C9,$09,$08,$C8,
  $D8,$18,$19,$D9,$1B,$DB,$DA,$1A,$1E,$DE,$DF,$1F,$DD,$1D,$1C,$DC,
  $14,$D4,$D5,$15,$D7,$17,$16,$D6,$D2,$12,$13,$D3,$11,$D1,$D0,$10,
  $F0,$30,$31,$F1,$33,$F3,$F2,$32,$36,$F6,$F7,$37,$F5,$35,$34,$F4,
  $3C,$FC,$FD,$3D,$FF,$3F,$3E,$FE,$FA,$3A,$3B,$FB,$39,$F9,$F8,$38,
  $28,$E8,$E9,$29,$EB,$2B,$2A,$EA,$EE,$2E,$2F,$EF,$2D,$ED,$EC,$2C,
  $E4,$24,$25,$E5,$27,$E7,$E6,$26,$22,$E2,$E3,$23,$E1,$21,$20,$E0,
  $A0,$60,$61,$A1,$63,$A3,$A2,$62,$66,$A6,$A7,$67,$A5,$65,$64,$A4,
  $6C,$AC,$AD,$6D,$AF,$6F,$6E,$AE,$AA,$6A,$6B,$AB,$69,$A9,$A8,$68,
  $78,$B8,$B9,$79,$BB,$7B,$7A,$BA,$BE,$7E,$7F,$BF,$7D,$BD,$BC,$7C,
  $B4,$74,$75,$B5,$77,$B7,$B6,$76,$72,$B2,$B3,$73,$B1,$71,$70,$B0,
  $50,$90,$91,$51,$93,$53,$52,$92,$96,$56,$57,$97,$55,$95,$94,$54,
  $9C,$5C,$5D,$9D,$5F,$9F,$9E,$5E,$5A,$9A,$9B,$5B,$99,$59,$58,$98,
  $88,$48,$49,$89,$4B,$8B,$8A,$4A,$4E,$8E,$8F,$4F,$8D,$4D,$4C,$8C,
  $44,$84,$85,$45,$87,$47,$46,$86,$82,$42,$43,$83,$41,$81,$80,$40 );


procedure getCRC16(p : pointer; len : word; var Hi : byte; var Lo : byte);
var
  index : word;
  i : word;
begin
  Hi:=$FF;
  Lo:=$FF;
  i:=0;
  While (i<Len) do
    begin
      Index:=Hi xor tByteArray(p^)[i];
      Hi:=Lo xor CRCHi[Index];
      Lo:=CRCLo[Index];

      i:=i+1;
    end;
end;

////////////////////////////////////TMBasicComPortMessage////////////////////////////
constructor TMBasicComPortMessage.Create();
begin
  FTableRecord := TMTableRecord.Create('Messages');

  Reset;

  FMessageUid := GenerateUid;
end;


destructor TMBasicComPortMessage.Destroy();
begin
  Reset;
  FTableRecord.Free;
  inherited;
end;

procedure TMBasicComPortMessage.Reset();
begin
  SetLength(FDataBytes, 0);

  FPriority     := mpNone;

  FDeviceId        := $00;
  FCommandId       := $00;
  FMSBRegisterAddr := $00;
  FLSBRegisterAddr := $00;

  FCRCHi     := $00;
  FCRCLo     := $00;

  FTableRecord.ClearRecordValues;
end;


function TMBasicComPortMessage.GenerateUid() : string;
var
  i : integer;
  randomIndex : integer;
const
  Uid_Length = 20;
  Uid_Alphabet = '0123456789abcdefgh';
begin
  Result := '';
  randomize;
  for i := 0 to Uid_Length - 1 do
    begin
      RandomIndex := random(Length(Uid_Alphabet));
      result := result + Uid_Alphabet[RandomIndex + 1];
    end;
end;



procedure TMBasicComPortMessage.LoadDataBytes(ADataBytes : TDynamicByteArray);
var
  len, i : integer;
begin
  len := Length(ADataBytes);

  if len = 0
    then Exit;

  SetLength(FDataBytes, len);

  for i := 0 to len - 1 do
    FDataBytes[i] := ADataBytes[i];
end;


procedure TMBasicComPortMessage.SaveDataBytes(var ADataBytes : TDynamicByteArray);
var
  len, i : integer;
begin
  len := Length(FDataBytes);

  if len = 0
    then Exit;

  SetLength(ADataBytes, len);

  for i := 0 to len - 1 do
    ADataBytes[i] := FDataBytes[i];
end;


function TMBasicComPortMessage.GenerateMessageString() : string;
var
  i, len : integer;
  AMessage : TDynamicByteArray;
begin
  GenerateMessage(AMessage);

  len := Length(AMessage);

  Result := '';
  for i := 0 to len - 1 do
    if i = (len - 1)
      then Result := Result + IntToHex(AMessage[i], 2)
      else Result := Result + IntToHex(AMessage[i], 2) + ' ';
end;

procedure TMBasicComPortMessage.Clear();
begin
  Reset;
end;

//////////////TMBasicComPortMessagesList//////////////////////////////////


end.
