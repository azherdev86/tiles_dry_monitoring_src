unit CConditions;

interface

uses CRecordFields;

type TMConditionType = (ctNone, ctLike, ctEqual, ctNotEqual, ctMore,
      ctLess, ctMoreEqual, ctLessEqual, ctStartingWith);

type TMOperationType = (otNone, otXOR, otOR, otAND);

//Условия в запросах после Where. Например WHERE (ID > 2)
type
  TMCondition = class (TMRecordField)
    constructor Create; override;
    destructor Destroy;  override;

  public
    ConditionType : TMConditionType;
    //нужно для того чтобы можно было осуществлять
    //фильтрацию не только по FieldName но и по FieldTitle
    QueryText : string;
    FieldTitle : string;
    procedure Reset; override;
    function GetConditionString : string;
    procedure CopyFromCondtion(Condition : TMCondition);
  private
    function ConditionTypeAsString : string;
  protected
    function GetValueString() : string; override;
end;


implementation

uses Variants, SysUtils, LApplicationGlobals, CDataBaseStructure;

//TMCondition

constructor TMCondition.Create;
begin
  inherited;
  Reset;
end;

destructor TMCondition.Destroy;
begin
  Reset;
  inherited;
end;

procedure TMCondition.Reset;
begin
  inherited;
  ConditionType := ctNone;
  QueryText := '';
  FieldTitle := '';
end;

function TMCondition.ConditionTypeAsString : string;
begin
  case ConditionType of
    ctNone: Result := '';
    ctEqual:
      begin
        if (Value = null) or VarIsEmpty(Value)
          then Result := ' IS '
          else Result := '=';
      end;
    ctNotEqual :
      begin
        if (Value = null) or VarIsEmpty(Value)
          then Result := ' IS NOT  '
          else Result := '<>';
      end;
    ctMore: Result := '>';
    ctLess: Result := '<';
    ctMoreEqual: Result := '>=';
    ctLessEqual: Result := '<=';
//    ctLike : if GLOBAL_DATABASE_TYPE = dtFireBird
//               then Result := ' CONTAINING '
//               else Result := ' LIKE ';
    ctStartingWith : Result := ' STARTING WITH ';
  end;
end;

function TMCondition.GetValueString() : string;
begin
  if (ConditionType <> ctLike) and (ConditionType <> ctStartingWith) 
    then Result := inherited GetValueString
    else
      begin
        if VarIsEmpty(Value) or (Value = null)
          then
            begin
              Result := 'null';
              Exit;
            end;

//        if GLOBAL_DATABASE_TYPE = dtFireBird
//          then Result := '''' + VarToStr(Value) + ''''
//          else Result := '''%' + VarToStr(Value) + '%''';
      end;
end;

function TMCondition.GetConditionString : string;
var
  ConditionEnd : string;
begin
  ConditionEnd :=  ConditionTypeAsString + ValueString;

  if QueryText <> ''
    then
      begin
        QueryText := StringReplace(QueryText, '"', '''', [rfReplaceAll]);
        Result := '(' + QueryText + ')' + ConditionEnd;
      end
    else
      begin
        if TableName <> ''
          then Result := TableName + '.' +
                         FieldName + ConditionEnd
          else Result := FieldName + ConditionEnd;
      end;
end;

procedure TMCondition.CopyFromCondtion(Condition : TMCondition);
begin
  if not Assigned(Condition)
    then Exit;

  ConditionType := Condition.ConditionType;
  QueryText     := Condition.QueryText;
  FieldTitle    := Condition.FieldTitle;

  CopyFromRecordField(Condition as TMRecordField);
end;

end.
