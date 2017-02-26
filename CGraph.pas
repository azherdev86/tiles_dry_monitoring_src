unit CGraph;

interface

uses Graphics, Classes, ExtCtrls, Types, CRows;

const
  GRID_ROW_COUNT = 6;
  GRID_COL_COUNT = 10;

  GRID_COL_WIDTH  = 80;
  GRID_ROW_HEIGHT = 80;

  GRID_WIDTH = 1;

  AXIS_MIN_Y  = 90;
  AXIS_MAX_Y  = 170;

  SERIES_LINE_WIDTH = 1;
  SERIES_POINT_RADIUS = 3;
  SERIES_COLOR = clBlack;

  SELECTED_CELL_COLOR    = $EEEEEE; //светло-серый
  HIGHLIGHTED_ROW_COLOR  = $D5E5FB; //светло-розовый
  HIGHLIGHTED_CELL_COLOR = $ACCBF7; //немного темнее, чем светло-розовый

type
  TRAxis = record
    MinY : single;
    MaxY : single;
  end;

type
  TRFloatPoints = record
    Value   : single;
    Section : integer;
  end;

type
  TFloatPointsArray = array of TRFloatPoints;

type
  TMSeries = class
    constructor Create();
    destructor Destroy; override;
  private
    FFloatPoints : TFloatPointsArray;
    FColor : TColor;
    FLineWidth : Integer;
    FPointRadius : Integer;
    FRow : Integer;

    procedure Reset;

  public
    procedure AddPoint(AValue : single; ASection : integer); overload;
    procedure AddPoint(AValue : single; ASection : integer; AColor : TColor); overload;
    procedure Clear;

    function GetPointsCount() : integer;

  public
    property FloatPoints : TFloatPointsArray read FFloatPoints;
    property Color : TColor read FColor write FColor;
    property LineWidth : integer read FLineWidth write FLineWidth;
    property PointRadius : integer read FPointRadius write FPointRadius;
    property Row : integer read FRow write FRow;
  end;

type
  TMSeriesList = class
    constructor Create;
    destructor Destroy; override;

  public
    function GetItem(ItemIndex : integer) : TMSeries;
    function AddItem(Item : TMSeries) : TMSeries;
    function DeleteItem(ItemIndex : integer) : boolean;

    function GetCount : integer;

    procedure Clear;

  private
    Items : TStringList;

    procedure Reset;
  end;

type
  TMGraph = class
    constructor Create();
    destructor Destroy; override;
  private
    FBitMap : TBitmap;
    FAxis   : TRAxis;

    FSeriesList : TMSeriesList;
    FRangeList  : TMSeriesList;

    FGridRowCount  : integer;
    FGridColCount  : integer;
    FGridColWidth  : integer;
    FGridRowHeight : integer;
    FGridWidth     : integer;

    FSelectedCell : TPoint;

    FNeedDrawSelectedCell : boolean;

    FHighLightedRows : TMRows;

    procedure Reset();

    procedure DrawWhiteScreen();
    procedure DrawGrid();
    procedure DrawSelectedCell();
    procedure DrawHighLightedCells();
    procedure DrawSeries();
    procedure DrawRanges();
    procedure DrawAverage();
    procedure DrawLegend();

    procedure DrawPoint(APointRadius, AX, AY : integer; AColor : TColor);

    function GetSeries(Index : integer) : TMSeries;
    function GetRange(Index : integer) : TMSeries;

    function GetBitMapWidth() : integer;
    function GetBitMapHeight() : integer;

    function CalculateRatio() : single;

    function IsHighLighted() : boolean;

  public
    procedure DrawGraph(PaintBox : TPaintBox);
    procedure SetAxisRanges(MinY, MaxY : single);
    procedure Clear();

    function AddSeries(ARow : integer) : TMSeries;
    function AddRange() : TMSeries;
    function LoadSettings() : boolean;

    function GetGridCoord(AMouseCoord : TPoint) : TPoint;

    procedure HighLightCell(ACell : TPoint);   //Подсветить ячейку
    procedure DeHighLightCell(ACell : TPoint); //Убрать подсветку

  public
    property Width : integer read GetBitMapWidth;
    property Height : integer read GetBitMapHeight;
    property Axis : TRAxis read FAxis;
    property SelectedSection : TPoint read FSelectedCell write FSelectedCell;
    property NeedDrawSelectedSection : boolean read FNeedDrawSelectedCell write FNeedDrawSelectedCell;
    property Series : TMSeriesList read FSeriesList;
    property Ranges : TMSeriesList read FRangeList;
  end;

implementation

uses SysUtils, Windows, LApplicationGlobals;

////////////////////////TMSeries/////////////////////////
constructor TMSeries.Create();
begin
  Reset;

  FColor       := SERIES_COLOR;
  FLineWidth   := SERIES_LINE_WIDTH;
  FPointRadius := SERIES_POINT_RADIUS;
end;


destructor TMSeries.Destroy;
begin
  Reset;

  inherited;
end;


procedure TMSeries.Reset;
begin
  SetLength(FFloatPoints, 0);

  FColor := clNone;
  FLineWidth := 0;
  FPointRadius := 0;
  FRow := 0;
end;


procedure TMSeries.AddPoint(AValue : single; ASection : integer);
var
  index : integer;
begin
  SetLength(FFloatPoints, Length(FFloatPoints) + 1);

  index := High(FFloatPoints);

  FFloatPoints[index].Value   := AValue;
  FFloatPoints[index].Section := ASection;
end;

procedure TMSeries.AddPoint(AValue : single; ASection : integer; AColor : TColor);
begin
  AddPoint(AValue, ASection);

  FColor := AColor;
end;

procedure TMSeries.Clear;
begin
  Reset;
end;


function TMSeries.GetPointsCount() : integer;
begin
  Result := Length(FFloatPoints);
end;

////////////////////////TMSeriesList////////////////////////////
constructor TMSeriesList.Create;
begin
  Items := TStringList.Create;
  Reset;
end;


destructor TMSeriesList.Destroy;
begin
  Reset;
  Items.Free;
  inherited;
end;


function TMSeriesList.GetItem(ItemIndex : integer) : TMSeries;
begin
  Result := nil;

  if ItemIndex < 0
    then Exit;

  Result := Items.Objects[ItemIndex] as TMSeries;
end;


function TMSeriesList.AddItem(Item : TMSeries) : TMSeries;
var
  Index : integer;
begin
  Result := nil;

  if not Assigned(Item)
    then Exit;

  Index := GetCount;

  if Items.AddObject(IntToStr(Index), Item) >= 0
    then Result := Item;
end;


function TMSeriesList.DeleteItem(ItemIndex : integer) : boolean;
var
  Series : TMSeries;
begin
  Result := FALSE;
  Series := GetItem(ItemIndex);
  if Assigned(Series) then
    begin
      Items.Delete(ItemIndex);
      Series.Free;
      Result := TRUE;
    end;
end;


procedure TMSeriesList.Reset;
var
  i : integer;
  Series : TMSeries;
begin
  for i := 0 to GetCount - 1 do
  begin
    Series := Items.Objects[i] as TMSeries;
    if Assigned(Series)
      then Series.Free;
  end;

  Items.Clear;
end;


function TMSeriesList.GetCount : integer;
begin
  Result := Items.Count;
end;

procedure TMSeriesList.Clear;
begin
  Reset;
end;

///////////////////////////TMGraph////////////////////////////


constructor TMGraph.Create();
begin
  FBitMap          := Graphics.TBitmap.Create;
  FSeriesList      := TMSeriesList.Create;
  FRangeList       := TMSeriesList.Create;
  FHighLightedRows := TMRows.Create;

  FGridRowCount  := GRID_ROW_COUNT;
  FGridColCount  := GRID_COL_COUNT;
  FGridColWidth  := GRID_COL_WIDTH;
  FGridRowHeight := GRID_ROW_HEIGHT;
  FGridWidth     := GRID_WIDTH;

  FBitMap.Width := FGridColCount*FGridColWidth + 1;
  FBitMap.Height := FGridRowCount*FGridRowHeight + 1;

  FAxis.MinY := AXIS_MIN_Y;
  FAxis.MaxY := AXIS_MAX_Y;
end;


destructor TMGraph.Destroy;
begin
  FBitMap.Free;
  FSeriesList.Free;
  FRangeList.Free;
  FHighLightedRows.Free;

  inherited;
end;

procedure TMGraph.Reset();
begin
  FSeriesList.Reset;

  DrawWhiteScreen;
end;

procedure TMGraph.DrawSelectedCell();
var
  Rect : TRect;
begin
  if not FNeedDrawSelectedCell
    then Exit;

  Rect.Left  := FSelectedCell.X*FGridColWidth;
  Rect.Right := Rect.Left + FGridColWidth + 1;

  Rect.Top := FSelectedCell.Y*FGridRowHeight;
  Rect.Bottom := Rect.Top + FGridRowHeight + 1;

  FBitMap.Canvas.Brush.Color := SELECTED_CELL_COLOR;
  FBitMap.Canvas.Rectangle(Rect);
end;

procedure TMGraph.DrawHighLightedCells();
var
  row_count, col_count,
  col_index, row_index,
  i, j : integer;

  Rect : TRect;

  Row : TMRow;
  Col : TMColumn;
begin
  if not IsHighLighted
    then Exit;

  row_count := FHighLightedRows.GetCount;

  for i := 0 to row_count - 1 do
    begin
      Row := FHighLightedRows.GetItem(i);

      if not Assigned(Row)
        then Continue;

      row_index := Row.RowIndex;

      //Закрашиваем ряд светло-розовым цветом
      Rect.Left  := 0;
      Rect.Right := GetBitMapWidth;

      Rect.Top := row_index*FGridRowHeight;
      Rect.Bottom := Rect.Top + FGridRowHeight + 1;

      FBitMap.Canvas.Brush.Color := HIGHLIGHTED_ROW_COLOR;
      FBitMap.Canvas.Rectangle(Rect);

      col_count := Row.GetCount;

      for j := 0 to col_count - 1 do
        begin
          Col := Row.GetItem(j);

          if not Assigned(Col)
            then Continue;

          col_index := Col.ColumnIndex;

          //Закрашиваем ячейку чуть более темным оттенком светло-розового
          Rect.Left  := col_index*FGridColWidth;
          Rect.Right := Rect.Left + FGridColWidth + 1;

          Rect.Top := row_index*FGridRowHeight;
          Rect.Bottom := Rect.Top + FGridRowHeight + 1;

          FBitMap.Canvas.Brush.Color := HIGHLIGHTED_CELL_COLOR;
          FBitMap.Canvas.Rectangle(Rect);
        end;
    end;
end;


procedure TMGraph.DrawWhiteScreen();
var
  Rect : TRect;
begin
  Rect.TopLeft     := Point(0, 0);
  Rect.BottomRight := Point(GetBitMapWidth, GetBitMapHeight);

  FBitMap.Canvas.Brush.Color := clWhite;
  FBitMap.Canvas.FillRect(Rect);
end;


procedure TMGraph.DrawGrid();
var
  i : integer;
begin
  FBitMap.Canvas.Pen.Width := FGridWidth;
  FBitMap.Canvas.Pen.Color := clBlack;
  FBitMap.Canvas.Pen.Style := psSolid;


  for i := 0 to FGridRowCount do
  begin
    FBitMap.Canvas.MoveTo(0, i*FGridRowHeight);
    FBitMap.Canvas.LineTo(FBitMap.Width, i*FGridRowHeight);
  end;

  for i := 0 to FGridColCount do
  begin
    FBitMap.Canvas.MoveTo(i*FGridColWidth, 0);
    FBitMap.Canvas.LineTo(i*FGridColWidth, FBitMap.Height);
  end;
end;

procedure TMGraph.DrawRanges();
var
  i, j, k,
  count,
  x_int_start, y_int_start,
  x_int_end, y_int_end     : integer;

  Range : TMSeries;
  Ratio : single;
begin
  count:= FRangeList.GetCount;

  if count <= 0
    then Exit;

  Ratio := CalculateRatio;

  FBitMap.Canvas.Brush.Color := clWhite;

  for i := 0 to count - 1 do
    begin
      Range := GetRange(i);

      if not Assigned(Range)
        then Continue;

      for j := 0 to FGridRowCount - 1 do
        begin
          for k := 0 to Range.GetPointsCount - 1 do
            begin
              if (Range.FFloatPoints[k].Value < FAxis.MinY) or
                 (Range.FFloatPoints[k].Value > FAxis.MaxY)
                then Continue;


//             FormMain.MemoLogs.Lines.Add(FloatToStr(Range.FFloatPoints[k].Value));
//              FormMain.Caption := FormMain.Caption + ' ' +  FloatToStr(Range.FFloatPoints[k].Value);


              FBitMap.Canvas.Pen.Color := Range.Color;
              FBitMap.Canvas.Pen.Width := Range.LineWidth;
              FBitMap.Canvas.Pen.Style := psDot;

              x_int_start := Range.FFloatPoints[k].Section*FGridColWidth;
              x_int_end   := x_int_start + FGridColWidth;

              y_int_start := FGridRowHeight*j + (FGridRowHeight - Round(Ratio*(Range.FFloatPoints[k].Value - FAxis.MinY)));
              y_int_end   := y_int_start;

              FBitMap.Canvas.MoveTo(x_int_start, y_int_start);
              FBitMap.Canvas.LineTo(x_int_end,   y_int_end);
            end;
        end;
    end;
end;

procedure TMGraph.DrawAverage();
var
  i, j, k, z,
  count, points_count,
  x_int_start, y_int_start,
  x_int_end, y_int_end     : integer;

  sum, average : single;

  Series : TMSeries;

  Ratio : single;
begin
  count:= FSeriesList.GetCount;

  if count <= 0
    then Exit;

  Ratio := CalculateRatio;

  FBitMap.Canvas.Brush.Color := clWhite;
  FBitMap.Canvas.Pen.Color := clBlack;
  FBitMap.Canvas.Pen.Width := 1;
  FBitMap.Canvas.Pen.Style := psSolid;

  for i := 0 to FGridRowCount - 1 do
    begin
      for j := 0 to FGridColCount - 1 do
        begin
          average := 0;
          sum := 0;

          for k := 0 to count - 1 do
            begin
              Series := GetSeries(k);

              if not Assigned(Series)
                then Continue;

              if Series.Row <> i
                then Continue;

              points_count := Series.GetPointsCount;

              for z := 0 to points_count - 1 do
                begin
                  if Series.FloatPoints[z].Section <> j
                    then Continue;

                  sum := sum + Series.FloatPoints[z].Value;
                  average := average + 1;
                end;
            end;

            if average <> 0
              then average := sum/average
              else average := 0;

            if (average < FAxis.MinY) or
               (average > FAxis.MaxY)
              then Break;


            x_int_start := j*FGridColWidth;
            x_int_end   := x_int_start + FGridColWidth;

            y_int_start := i*FGridRowHeight + (FGridRowHeight - Round(Ratio*(average - FAxis.MinY)));
            y_int_end   := y_int_start;

            FBitMap.Canvas.MoveTo(x_int_start, y_int_start);
            FBitMap.Canvas.LineTo(x_int_end,   y_int_end);
        end;
    end;

end;


procedure TMGraph.DrawSeries();
var
  i, j,
  ser_count,
  points_count,
  x_int, y_int,
  radius, row : integer;

  tmpSeries : TMSeries;

  color : TColor;

  Ratio : single;
begin
  ser_count := FSeriesList.GetCount;

  FBitMap.Canvas.Brush.Color := clWhite;

  Ratio := CalculateRatio;

  for i := 0 to ser_count - 1 do
    begin
      tmpSeries := GetSeries(i);

      if not Assigned(tmpSeries)
        then Continue;

      points_count := tmpSeries.GetPointsCount;

      if points_count = 0
        then Continue;

      row    := tmpSeries.Row;
      color  := tmpSeries.Color;
      radius := tmpSeries.PointRadius;
      x_int  := 0;
      y_int  := 0;

      for j :=  0 to points_count - 1 do
        begin
          if (tmpSeries.FloatPoints[j].Value >= FAxis.MinY) and
             (tmpSeries.FloatPoints[j].Value <= FAxis.MaxY)
            then y_int := FGridRowHeight*row + (FGridRowHeight - Round(Ratio*(tmpSeries.FloatPoints[j].Value - FAxis.MinY)))
            else Continue;

          case color of
            clRed   : x_int := FGridColWidth*j + 1*(FGridColWidth div 5);
            clGreen : x_int := FGridColWidth*j + 2*(FGridColWidth div 5);
            clBlue  : x_int := FGridColWidth*j + 3*(FGridColWidth div 5);
            clAqua  : x_int := FGridColWidth*j + 4*(FGridColWidth div 5);
          end;

          DrawPoint(radius, x_int, y_int, color);
        end;

    end;
end;


procedure TMGraph.DrawLegend();
begin

end;


function TMGraph.GetSeries(Index : integer) : TMSeries;
begin
  Result := FSeriesList.GetItem(Index);
end;


function TMGraph.GetRange(Index : integer) : TMSeries;
begin
  Result := FRangeList.GetItem(Index);
end;


procedure TMGraph.DrawPoint(APointRadius, AX, AY : integer; AColor : TColor);
begin
  FBitMap.Canvas.Brush.Color := AColor;

  FBitMap.Canvas.Ellipse(AX - APointRadius,
                         AY - APointRadius,
                         AX + APointRadius,
                         AY + APointRadius);
end;

function TMGraph.AddSeries(ARow : integer) : TMSeries;
var
  tmpSeries : TMSeries;
begin
  tmpSeries := TMSeries.Create;

  tmpSeries.Row := ARow;

  Result := FSeriesList.AddItem(tmpSeries);
end;


function TMGraph.AddRange() : TMSeries;
var
  tmpRange : TMSeries;
begin
  tmpRange := TMSeries.Create;

  Result := FRangeList.AddItem(tmpRange);
end;

function TMGraph.LoadSettings() : boolean;
var
  RangeMin : TMSeries;
  RangeMax : TMSeries;

  MinYValue,
  MaxYValue : single;
begin
  FAxis.MinY := ApplicationProgramSettings.GraphSettings.AxisMinYValue;
  FAxis.MaxY := ApplicationProgramSettings.GraphSettings.AxisMaxYValue;

  FRangeList.Reset;

  //Серия 1
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section1RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section1RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 0, clRed);
  RangeMax.AddPoint(MaxYValue, 0, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;


  //Серия 2
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section2RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section2RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 1, clRed);
  RangeMax.AddPoint(MaxYValue, 1, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 3
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section3RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section3RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 2, clRed);
  RangeMax.AddPoint(MaxYValue, 2, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 4
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section4RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section4RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 3, clRed);
  RangeMax.AddPoint(MaxYValue, 3, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 5
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section5RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section5RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 4, clRed);
  RangeMax.AddPoint(MaxYValue, 4, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;


  //Серия 6
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section6RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section6RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 5, clRed);
  RangeMax.AddPoint(MaxYValue, 5, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 7
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section7RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section7RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 6, clRed);
  RangeMax.AddPoint(MaxYValue, 6, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;


  //Серия 8
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section8RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section8RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 7, clRed);
  RangeMax.AddPoint(MaxYValue, 7, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 9
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section9RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section9RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 8, clRed);
  RangeMax.AddPoint(MaxYValue, 8, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;

  //Серия 10
  RangeMin := TMSeries.Create;
  RangeMax := TMSeries.Create;

  MinYValue := ApplicationProgramSettings.GraphSettings.Section10RangeMinYValue;
  MaxYValue := ApplicationProgramSettings.GraphSettings.Section10RangeMaxYValue;

  RangeMin.AddPoint(MinYValue, 9, clRed);
  RangeMax.AddPoint(MaxYValue, 9, clRed);

  RangeMin := FRangeList.AddItem(RangeMin);
  if not Assigned(RangeMin)
    then RangeMin.Free;

  RangeMax := FRangeList.AddItem(RangeMax);
  if not Assigned(RangeMax)
    then RangeMax.Free;


  Result := TRUE;
end;


function TMGraph.GetGridCoord(AMouseCoord : TPoint) : TPoint;
var
  row, col : integer;
begin
  col := AMouseCoord.X div FGridColWidth;
  row := AMouseCoord.Y div FGridRowHeight;

  Result := Point(col, row);
end;


procedure TMGraph.HighLightCell(ACell : TPoint);
var
  row_index, col_index : integer;

  Row : TMRow;
  Column : TMColumn;
begin
  col_index := ACell.X;
  row_index := ACell.Y;

  if (col_index < 0) or (col_index > (FGridColCount - 1))
    then Exit;

  if (row_index < 0) or (row_index > (FGridRowCount - 1))
    then Exit;

  Row := FHighLightedRows.GetItem(IntToStr(row_index));

  if not Assigned(Row)
    then
      begin
        Row := TMRow.Create;
        Row.RowIndex := row_index;

        Row := FHighLightedRows.AddItem(Row);

        if not Assigned(Row)
          then Exit;
      end;

  Column := Row.GetItem(IntToStr(col_index));

  if Assigned(Column)
    then Row.DeleteItem(IntToStr(Column.ColumnIndex));

  Column := TMColumn.Create;
  Column.ColumnIndex := col_index;

  Row.AddItem(Column);
end;


procedure TMGraph.DeHighLightCell(ACell : TPoint);
var
  row_index, col_index : integer;

  Row : TMRow;
  Column : TMColumn;
begin
  col_index := ACell.X;
  row_index := ACell.Y;

  if (col_index < 0) or (col_index > (FGridColCount - 1))
    then Exit;

  if (row_index < 0) or (row_index > (FGridRowCount - 1))
    then Exit;

  Row := FHighLightedRows.GetItem(IntToStr(row_index));

  if not Assigned(Row)
    then Exit;

  if Row.GetCount = 0
    then Exit;

  Column := Row.GetItem(IntToStr(col_index));

  if Assigned(Column)
    then Row.DeleteItem(IntToStr(Column.ColumnIndex));

  if Row.GetCount = 0
    then FHighLightedRows.DeleteItem(IntToStr(Row.RowIndex));
end;

function TMGraph.GetBitMapWidth() : integer;
begin
  Result := FBitMap.Width;
end;


function TMGraph.GetBitMapHeight() : integer;
begin
  Result := FBitMap.Height;
end;

function TMGraph.CalculateRatio() : single;
begin
  Result := FGridRowHeight/(Axis.MaxY - Axis.MinY);
end;

function TMGraph.IsHighLighted() : boolean;
begin
  Result := (FHighLightedRows.GetCount > 0);
end;


procedure TMGraph.DrawGraph(PaintBox : TPaintBox);
begin
  DrawWhiteScreen;     //заполняем весь холст белой заливкой
  DrawHighLightedCells;
  DrawSelectedCell; //перерисовка выделенной области
  DrawGrid;            //координатная сетка
  DrawAverage;         //отрисовка линий со средними значениями
  DrawSeries;          //отрисовка серий
  DrawRanges;          //отрисовка границ
  DrawLegend;          //отрисовка легенды

  PaintBox.Canvas.Draw(0, 0, FBitMap); //отрисовка BitMap на внешней Canvas
end;


procedure TMGraph.SetAxisRanges(MinY, MaxY : single);
begin
  FAxis.MinY := MinY;
  FAxis.MaxY := MaxY;
end;

procedure TMGraph.Clear;
begin
  Reset;
end;

end.
