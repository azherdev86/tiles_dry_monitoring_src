unit COutgoingComPortMessage;

interface

uses Classes, CBasicComPortMessage;

const
  TIMEOUT_SEND_MESSAGE = 200;

type
  TypeOutgoingMessageState = (omsNone,
                              omsWaitResponse, //��������� ����������. ��������� �������������
                              omsDelievered); //��������� ����������

  TypeOutgoingMessageError = (omeNone,
                              omeTimeout);

type
  TMOutgoingComportMessage = class (TMBasicComPortMessage)

  private
    FState : TypeOutgoingMessageState;
    FError : TypeOutgoingMessageError;

    FCreationTime   : TDateTime;
    FSentTime       : TDateTime;
    FDelieveredTime : TDateTime;

    FTimeOut         : integer; //������������
    FSentTimeCounter : integer;

  protected
    procedure Reset(); override;

  public
    procedure GenerateMessage(var AMessageBytes : TDynamicByteArray); override;

  public
    property State : TypeOutgoingMessageState read FState write FState;
    property Error : TypeOutgoingMessageError read FError write FError;

    property CreationTime   : TDateTime read FCreationTime   write FCreationTime;
    property SentTime       : TDateTime read FSentTime       write FSentTime;
    property DelieveredTime : TDateTime read FDelieveredTime write FDelieveredTime;

    property SentTimeCounter : integer read FSentTimeCounter write FSentTimeCounter;
    property TimeOut         : integer read FTimeOut;
  end;


type
  TMOutgoingComportMessagesList = class
    constructor Create;
    destructor  Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMOutgoingComportMessage; overload;
    function GetItem(ItemName  : string)  : TMOutgoingComportMessage; overload;

    function AddItem(Item : TMOutgoingComportMessage) : TMOutgoingComportMessage;
    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemName : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

    function GetMessageToSend() : TMOutgoingComportMessage;
  private
    Items : TStringList;

    FSendingComPortMessage : TMOutgoingComportMessage;

    FMaxMessagesCount : integer;

    FLastMessageError : boolean;
    FLastSendingDeviceId : Byte;

    function GetHighPriorityMessage()   : TMOutgoingComportMessage;
    function GetNormalPriorityMessage() : TMOutgoingComportMessage;

    procedure SetSendingComPortMessage(AComPortMessage : TMOutgoingComportMessage);

  protected
    procedure Reset;

  public
    property MaxMessagesCount : integer read FMaxMessagesCount;
    property SendingComPortMessage : TMOutgoingComportMessage read FSendingComPortMessage write SetSendingComPortMessage;
    property LastMessageError : boolean read FLastMessageError write FLastMessageError;
    property LastSendingDeviceId : Byte read FLastSendingDeviceId write FLastSendingDeviceId;

  end;

implementation

uses SysUtils;


procedure TMOutgoingComportMessage.Reset;
begin
  inherited;

  FState := omsNone;
  FError := omeNone;

  FCreationTime   := 0;
  FSentTime       := 0;
  FDelieveredTime := 0;

  FSentTimeCounter := 0;
end;

procedure TMOutgoingComportMessage.GenerateMessage(var AMessageBytes : TDynamicByteArray);
var
  len,
  i : integer;
begin
  len := Length(FDataBytes) + 4;

  SetLength(AMessageBytes, len);

  AMessageBytes[0] := FDeviceId;
  AMessageBytes[1] := FCommandId;
  AMessageBytes[2] := FMSBRegisterAddr;
  AMessageBytes[3] := FLSBRegisterAddr;     

  for i := 4 to len - 1 do
    AMessageBytes[i] := FDataBytes[i-4];

  getCRC16(AMessageBytes, len, FCRCHi, FCRCLo);

  len := len + 2;

  SetLength(AMessageBytes, len);

  AMessageBytes[len - 2] := FCRCHi;
  AMessageBytes[len - 1] := FCRCLo;
end;


////////////////////////////TMOutgoingComPortMessagesList/////////////////////////////

function TMOutgoingComportMessagesList.GetMessageToSend() : TMOutgoingComportMessage;
begin
  //������� ������� ����� ��������� � ������� �����������
  Result := GetHighPriorityMessage;

  if Assigned(Result)
    then Exit;

  Result := GetNormalPriorityMessage;
end;


function TMOutgoingComPortMessagesList.GetNormalPriorityMessage() : TMOutgoingComportMessage;
var
  i, count : integer;

  ComPortMessage : TMOutgoingComportMessage;

  MinCreationTime : TDateTime;
  MessageUid     : string;
begin
  Result := nil;

  count := GetCount;

  MinCreationTime := Now;
  MessageUid := '';

  for i := 0 to count - 1 do
  begin
    ComPortMessage := GetItem(i) as TMOutgoingComportMessage;

    if ComPortMessage.State = (omsDelievered)
      then Continue;

    if ComPortMessage.Priority = mpNormal
      then
        begin
          if ComPortMessage.CreationTime < MinCreationTime
            then
              begin
                MinCreationTime := ComPortMessage.CreationTime;
                MessageUid      := ComPortMessage.MessageUid;
              end;
        end;
  end;

  if MessageUid <> ''
    then Result := GetItem(MessageUid) as TMOutgoingComportMessage;
end;

procedure TMOutgoingComportMessagesList.SetSendingComPortMessage(AComPortMessage : TMOutgoingComportMessage);
begin
  if not Assigned(AComPortMessage)
    then FSendingComPortMessage := nil
    else
      begin
        FSendingComPortMessage := AComPortMessage;
        FLastSendingDeviceId   := AComPortMessage.DeviceId;
        FLastMessageError      := False;
      end;     
end;


function TMOutgoingComportMessagesList.GetHighPriorityMessage() : TMOutgoingComportMessage;
var
  i, count : integer;

  ComPortMessage : TMOutgoingComportMessage;

  MinCreationTime : TDateTime;
  MessageUid     : string;
begin
  Result := nil;

  count := GetCount;

  MinCreationTime := Now;
  MessageUid := '';

  for i := 0 to count - 1 do
  begin
    ComPortMessage := GetItem(i) as TMOutgoingComportMessage;

    if ComPortMessage.State = (omsDelievered)
      then Continue;

    if ComPortMessage.Priority = mpHigh
      then
        begin
          if ComPortMessage.CreationTime < MinCreationTime
            then
              begin
                MinCreationTime := ComPortMessage.CreationTime;
                MessageUid      := ComPortMessage.MessageUid;
              end;
        end;
  end;

  if MessageUid <> ''
    then Result := GetItem(MessageUid) as TMOutgoingComportMessage;
end;

constructor TMOutgoingComPortMessagesList.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMOutgoingComPortMessagesList.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMOutgoingComPortMessagesList.GetItem(ItemIndex : integer) : TMOutgoingComportMessage;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMOutgoingComportMessage;
end;

function TMOutgoingComPortMessagesList.GetItem(ItemName : string) : TMOutgoingComportMessage;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemName);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;


function TMOutgoingComPortMessagesList.AddItem(Item : TMOutgoingComportMessage) : TMOutgoingComportMessage;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  if Items.AddObject(Item.MessageUid, Item) >= 0
    then Result := Item;
end;


function TMOutgoingComPortMessagesList.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMOutgoingComportMessage;
begin
  Result := FALSE;
  Item := GetItem(ItemIndex);
  if Assigned(Item) then
    begin
      Items.Delete(ItemIndex);
      Item.Free;
      Result := TRUE;
    end;
end;

function TMOutgoingComPortMessagesList.DeleteItem(ItemName : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemName);

  Result := DeleteItem(ItemIndex);
end;


procedure TMOutgoingComPortMessagesList.Reset;
var
  i : integer;
  Item : TMOutgoingComportMessage;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMOutgoingComportMessage;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;

  FLastMessageError    := False;
  FLastSendingDeviceId := $00;


  FMaxMessagesCount := MAX_MESSAGES_COUNT;
end;

function TMOutgoingComPortMessagesList.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMOutgoingComPortMessagesList.Clear;
begin
  Reset;
end;





end.
