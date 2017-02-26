unit CTempValuesBuffer;

interface

uses Classes;

type
  TMTempBufferValue = class
    constructor Create();
    destructor Destroy();override;

  private
    FSensorId       : integer;
    FConveyorNumber : integer;
    FSectionNumber  : integer;
    FBoxNumber      : integer;
    FTempValue      : single;
    FTempTime       : TDateTime;

    FSensorPosition : string;

    procedure Reset();
  public
    property SensorId       : integer   read FSensorId       write FSensorId;
    property ConveyorNumber : integer   read FConveyorNumber write FConveyorNumber;
    property SectionNumber  : integer   read FSectionNumber  write FSectionNumber;
    property BoxNumber      : integer   read FBoxNumber      write FBoxNumber;
    property TempValue      : single    read FTempValue      write FTempValue;
    property TempTime    : TDateTime    read FTempTime       write FTempTime;

    property SensorPosition : string read FSensorPosition write FSensorPosition;
end;

type
  TMTempBufferValuesList = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMTempBufferValue;  overload;
    function GetItem(ItemTitle : string) : TMTempBufferValue; overload;
    function GetItem(ASectionNumber  : integer;
                     AConveyorNumber : integer;
                     ASensorPosition : string) : TMTempBufferValue; overload;

    function GetAverage(ASectionNumber : integer; APair : string) : single; overload;
    function GetAverage(ASectionNumber : integer; AConveyorNumber : integer; APair : string) : single; overload;

    function AddItem(Item : TMTempBufferValue) : TMTempBufferValue;
    function DeleteItem(ItemIndex : integer) : boolean; overload;
    function DeleteItem(ItemTitle : string) : boolean; overload;

    function GetCount : integer;

    procedure Clear;

  private
    Items : TStringList;

    procedure Reset;
  end;

implementation

uses SysUtils;


//////////////////////////CTempBufferValue/////////////////////////

constructor TMTempBufferValue.Create;
begin
  Reset;
end;

destructor TMTempBufferValue.Destroy;
begin
  Reset;
  inherited;
end;

procedure TMTempBufferValue.Reset;
begin
  FSensorId       := 0;
  FConveyorNumber := 0;
  FSectionNumber  := 0;
  FBoxNumber      := 0;
  FTempValue      := 0;
  FTempTime       := 0;

  FSensorPosition := '';
end;


//////////////TMTempBufferValuesList//////////////////////////////////
constructor TMTempBufferValuesList.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMTempBufferValuesList.Destroy;
begin
  Reset;
  Items.Free;
  inherited Destroy;
end;


function TMTempBufferValuesList.GetItem(ItemIndex : integer) : TMTempBufferValue;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMTempBufferValue;
end;

function TMTempBufferValuesList.GetItem(ItemTitle : string) : TMTempBufferValue;
var
  ItemIndex : integer;
begin
  Result := nil;

  ItemIndex := Items.IndexOf(ItemTitle);

  if ItemIndex >= 0
    then Result := GetItem(ItemIndex);
end;

function TMTempBufferValuesList.GetItem(ASectionNumber  : integer;
                                        AConveyorNumber : integer;
                                        ASensorPosition : string) : TMTempBufferValue;
var
  i, count : integer;

  Item : TMTempBufferValue;
begin
  Result := nil;

  count := GetCount;

  for i := 0 to count - 1 do
  begin
    Item := GetItem(i);

    if (Item.SectionNumber  = ASectionNumber) and
       (Item.ConveyorNumber = AConveyorNumber) and
       (Item.SensorPosition = ASensorPosition)
      then
        begin
          Result := Item;

          Break;
        end;
  end;
end;

function TMTempBufferValuesList.GetAverage(ASectionNumber : integer; APair : string) : single;
var
  Item : TMTempBufferValue;
  i, count : integer;

  sum : single;
begin
  Result := 0;

  sum := 0; count := 0;

  for i := 0 to GetCount - 1 do
    begin
      Item := GetItem(i);

      if not Assigned(Item)
        then continue;

      if (Item.FSectionNumber = ASectionNumber) and
         (Pos(APair, Item.FSensorPosition)<>0)
        then
          begin
            sum := sum + Item.TempValue;
            count := count + 1;
          end;
    end;

    if count <> 0
      then Result := sum/count;
end;

function TMTempBufferValuesList.GetAverage(ASectionNumber : integer; AConveyorNumber : integer; APair : string) : single;
var
  Item : TMTempBufferValue;
  i, count : integer;

  sum : single;
begin
  Result := 0;

  sum := 0; count := 0;

  for i := 0 to GetCount - 1 do
    begin
      Item := GetItem(i);

      if not Assigned(Item)
        then continue;

      if (Item.FSectionNumber = ASectionNumber) and
         (Item.FConveyorNumber = AConveyorNumber) and
         (Pos(APair, Item.FSensorPosition)<>0)
        then
          begin
            sum := sum + Item.TempValue;
            count := count + 1;
          end;
    end;

    if count <> 0
      then Result := sum/count;
end;


function TMTempBufferValuesList.AddItem(Item : TMTempBufferValue) : TMTempBufferValue;
var
  PreviosItem : TMTempBufferValue;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  PreviosItem := GetItem(IntToStr(Item.FSensorId));

  if Assigned(PreviosItem)
    then DeleteItem(IntToStr(PreviosItem.FSensorId));

   if Items.AddObject(IntToStr(Item.SensorId), Item) >= 0
    then Result := Item;
end;


function TMTempBufferValuesList.DeleteItem(ItemIndex : integer) : boolean;
var
  Item : TMTempBufferValue;
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

function TMTempBufferValuesList.DeleteItem(ItemTitle : string) : boolean;
var
  ItemIndex : integer;
begin
  ItemIndex := Items.IndexOf(ItemTitle);

  Result := DeleteItem(ItemIndex);
end;


procedure TMTempBufferValuesList.Reset;
var
  i : integer;
  Item : TMTempBufferValue;
begin
  for i := 0 to GetCount - 1 do
  begin
    Item := Items.Objects[i] as TMTempBufferValue;
    if Assigned(Item)
      then Item.Free;
  end;

  Items.Clear;
end;

function TMTempBufferValuesList.GetCount : integer;
begin
  Result := Items.Count;
end;


procedure TMTempBufferValuesList.Clear;
begin
  Reset;
end;


end.
