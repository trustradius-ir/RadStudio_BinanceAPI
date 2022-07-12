unit BinanceAPI;

interface
uses  System.Classes,IdTCPConnection, IdTCPClient, IdIOHandler,System.DateUtils,System.ZLib,System.Hash,TypInfo,IdURI,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,System.NetEncoding,SysUtils,IdGlobal, IdHTTP,RestAPI, JSon;


type
  TSymbolStatus = (PRE_TRADING,TRADING,POST_TRADING,END_OF_DAY);
  TSymbolType = (SPOT);
  TOrderStatus = (NEW,PARTIALLY_FILLED,FILLED,CANCELED,PENDING_CANCEL,REJECTED,EXPIRED);
  TOCOStatus = (RESPONSE,EXEC_STARTED,ALL_DONE);
//  TOCOOrderStatus = (EXECUTING,ALL_DONE,REJECT);
  TOrderType = (LIMIT,MARKET,STOP_LOSS,STOP_LOSS_LIMIT,TAKE_PROFIT,TAKE_PROFIT_LIMIT,LIMIT_MAKER);
  TOrderSide = (BUY,SELL);
  TTimeInForce = (GTC,IOC,FOK);
  TCandleInterval  = (C1m,C3m,C5m,C15m,C30m,C1h,C2h,C4h,C6h,C8h,C12h,C1d,C3d,C1w);

  TOrderListResult = record
    Symbol:String;
    OrderId:Int64;
    clientOrderId:String;
    Price:Real;
    origQty:Real;
    OrderStatus:TOrderStatus;
    OrderType:TOrderType;
    OrderSide:TOrderSide;
    StopPrice:Real;
    OrderTime:TDateTime;
    IsWorking:Boolean;
  end;
  TOrderListResultArray = Array of TOrderListResult;

  TCandleResult = record
    OpenTime:TDateTime;
    Open:Real;
    High:Real;
    Low:Real;
    Close:Real;
    Volume:Real;
    CloseTime:TDateTime;
    Quote:Real;
    TotalTrade:Int64;
    TakerBuyBaseVolume:Real;
    TakerBuyQuoteVolume:Real;
    Ignore:Real;
  end;
  TCandleResultArray = Array of TCandleResult;

  TMinMaxResult = record
    Min:Real;
    Mid:Real;
    Max:Real;
    CRed,CGreen:Integer;
    HGreen:Integer;
    ISReady:Boolean;
    LastUpdate:TDateTime;
  end;

  TNewOrderResult = record
    Result:Boolean;
    Symbol:String;
    OrderId:Int64;
    ClientOrderId:String;
    TransactTime:TDateTime;
    Price:Real;
    OrigQty:Real;
    ExecutedQty:Real;
    CummulativeQuoteQty:Real;
    OrderStatus:TOrderStatus;
    OrderType:TOrderType;
    OrderSide:TOrderSide;
    FillPrice:Real;
    FillQuantity:Real;
    FillCommission:Real;
  end;

  TNewOrderParams = record
    Symbol:String;
    OrderSide:TOrderSide;
    OrderType:TOrderType;
    TimeInForce:TTimeInForce;
    Quantity:real;
    QuoteOrderQty:real;
    Price:real;
    NewClientOrderId:String;
    StopPrice:real;
  end;

  TQueryOrderParams = record
    Symbol:String;
    OrderId:Int64;
    OrigClientOrderId:String;
  end;

  TTradFee = record
    Symbol:String;
    Maker,Taker:Real;
  end;

  TBinanceAPI = class
  protected
    FAPIUrlAddress,FApiKey,FSecretKey:String;
    FRestAPI: TRestAPI;
    FCustomHeaders:TStringList;
    FListenKey:String;
    function CheckHttpOK(HttpHeaders:String):Boolean;
    function GetHMac(AData,AKey:String):String;
    function GetSignature(AData:String):String;
    function GetServerTime:String;
    function GetSignedQuery(InQuery:String = ''):String;
  public
    function TradeAllOrder(Symbol:String = 'BTCUSDT'):TOrderListResultArray;
    function TradeCurrentOpenOrders(Symbol:String = 'BTCUSDT'):TOrderListResultArray;
    function TradeCancelOrder(QueryOrderParams:TQueryOrderParams;var OrderListResult:TOrderListResult):Boolean;
    function TradeQueryOrder(QueryOrderParams:TQueryOrderParams;var OrderListResult:TOrderListResult):Boolean;
    function TradeNewOrder(var HttpResult:String;var NewOrderResult:TNewOrderResult;OrderParams:TNewOrderParams):Boolean;
    function GetSymbolAveragePrice(Symbol:String = 'BTCUSDT'):real;
    function GetTradeFee(Symbol:String = 'BTCUSDT'):TTradFee;
    function GetAccountInfo(var AccountInfo:String):Boolean;
    function GetAccountBalance(var AccountBalance:Real;Symbol:String = 'BTC';CalcLocked:Boolean = True):Boolean;
    function GetSymbolMinMaxPriceLast(NumOfLastCandle:Integer = 10;Symbol:String = 'BTCUSDT';CandleInterval:TCandleInterval = C1h):TMinMaxResult;
    function GetSymbolKLines(Symbol:String = 'BTCUSDT';CandleInterval:TCandleInterval = C1h;Limit:Integer = 30):TCandleResultArray;
    function CreateListenKey:String;
    function PingListenKey:Boolean;
    function CloseListenKey:Boolean;
    constructor Create(ApiKey,SecretKey:String;APIUrlAddress:String = 'https://api.binance.com');
    destructor Destroy; override;
  end;
  Function FloatToStrFormated(InNumber:Real): string;
  Function StrToFloatBinance(InNumber:String): Real;
  Function FloatToStrFormatedDec(InNumber:Real;DecimalCount:Integer): string;
implementation

Function FloatToStrFormated(InNumber:Real): string;
var I:Integer;
  Temp:String;
begin
  Temp := FloatToStrF(InNumber,ffFixed,10,10);
  I := Length(Temp);
  While (Temp[I] = '0') And (I > 0) Do
  begin
    Delete(Temp,I,1);
    I := Length(Temp);
  end;
  if Temp[I] = '.' then Delete(Temp,I,1);
  Result := Temp;
end;

Function FloatToStrFormatedDec(InNumber:Real;DecimalCount:Integer): string;
var I:Integer;
  Temp:String;
begin
  Temp := FloatToStrF(InNumber,ffFixed,10,10);
  I := Length(Temp);
  While (Temp[I] = '0') And (I > 0) Do
  begin
    Delete(Temp,I,1);
    I := Length(Temp);
  end;
  if Temp[I] = '.' then Delete(Temp,I,1);
  I := Pos('.',Temp);
  if I > -1 then
  begin
    Delete(Temp,I+1+DecimalCount,100);
  end;
  Result := Temp;
end;


Function StrToFloatBinance(InNumber:String): Real;
var Temp:String;
begin
  Temp := Format('%.6f', [StrToFloat(InNumber)]);
  Result := StrToFloat(Temp);
end;

function TBinanceAPI.CheckHttpOK(HttpHeaders:String):Boolean;
var Headers:TStringList;
begin
  Result := False;
  try
    Headers :=  TStringList.Create;
    Headers.Text := HttpHeaders;
    if Headers.Count > 0 then
      if Pos('200',Headers.Strings[0]) > 0 then Result := True;
  finally
    Headers.Free;
  end;
end;

function TBinanceAPI.GetHMac(AData,AKey:String):String;
var HashSHA2:THashSHA2;
begin
  HashSHA2 := THashSHA2.Create();
  Result := HashSHA2.GetHMAC(AData,AKey);
end;

function TBinanceAPI.GetSignature(AData:String):String;
begin
  Result := GetHMac(AData,FSecretKey);
end;

function TBinanceAPI.GetSignedQuery(InQuery:String = ''):String;
begin
  InQuery := 'timestamp='+GetServerTime+InQuery;
  Result := InQuery + '&signature='+GetSignature(InQuery);
end;

constructor TBinanceAPI.Create(ApiKey,SecretKey:String;APIUrlAddress:String = 'https://api.binance.com');
begin
  inherited Create;
  FListenKey := '';

  FAPIUrlAddress := APIUrlAddress;
  FApiKey := ApiKey;
  FSecretKey := SecretKey;

  FRestAPI := TRestAPI.Create;

  FCustomHeaders := TStringList.Create;
  FCustomHeaders.Add('X-MBX-APIKEY='+ApiKey);
  FCustomHeaders.Add('Content-Type=application/x-www-form-urlencoded');
end;

destructor TBinanceAPI.Destroy;
begin
  inherited Destroy;
  FRestAPI.Destroy;
end;

function TBinanceAPI.GetServerTime:String;
var HttpResult,HttpHeaders:String;
    JSONValue:TJSONValue;
    A:String;
begin
  HttpResult := '';
  try
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/time',HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
        try
          JSONValue := TJSonObject.ParseJSONValue(HttpResult);
          Result := JSONValue.GetValue<string>('serverTime');
        finally
          JSONValue.Free;
        end;
    end;
  except
    Result := IntToStr(DateTimeToUnix(Now,False) * 1000);
  end;
end;

function TBinanceAPI.CreateListenKey:String;
var HttpResult,HttpHeaders:String;
    JSONValue:TJSONValue;
begin
  HttpResult := '';
  try
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/userDataStream',HttpHeaders,HttpResult,cmPost,FCustomHeaders) then
    begin
      try
        JSONValue := TJSonObject.ParseJSONValue(HttpResult);
        FListenKey := JSONValue.GetValue<string>('listenKey');
        Result := FListenKey;
      finally
        JSONValue.Free;
      end;
    end;
  except
    Result := '';
  end;
end;

function TBinanceAPI.CloseListenKey:Boolean;
var HttpHeaders,HttpResult:String;
    APIQuery:String;
begin
  HttpResult := '';
  Result := False;
  try
    APIQuery := 'listenKey='+FListenKey;
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/userDataStream?'+APIQuery,HttpHeaders,HttpResult,cmDelete,FCustomHeaders) then
    begin
      Result := True;
    end;
  except
  end;
end;


function TBinanceAPI.PingListenKey:Boolean;
var HttpHeaders,HttpResult:String;
    APIQuery:String;
begin
  HttpResult := '';
  Result := False;
  try
    APIQuery := 'listenKey='+FListenKey;
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/userDataStream?'+APIQuery,HttpHeaders,HttpResult,cmPut,FCustomHeaders) then
    begin
      Result := True;
    end;
  except
  end;
end;

function TBinanceAPI.GetAccountInfo(var AccountInfo:String):Boolean;
var HttpHeaders,HttpResult:String;
    JSONValue:TJSONValue;
    APIQuery:String;
begin
  Result := False;
  HttpResult := '';
  AccountInfo := '{}';
  try
    APIQuery := '';
    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/account?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      Result := CheckHttpOK(HttpHeaders);
      AccountInfo := HttpResult;
    end;
  except
    AccountInfo := '{}';
  end;
end;

function TBinanceAPI.GetAccountBalance(var AccountBalance:Real;Symbol:String = 'BTC';CalcLocked:Boolean = True):Boolean;
var ReceivedData:String;
    Balance_Asset,Balance_Free,Balance_Locked:String;
    JSONValue:TJSONValue;
    JSONArray:TJSONArray;
begin
  Result := False;
  AccountBalance := 0;
  try
    if not GetAccountInfo(ReceivedData) Then Exit;
    try
      JSONValue := TJSonObject.ParseJSONValue(ReceivedData);
      JSONArray := JSONValue.GetValue<TJSONArray>('balances');

      for JSONValue in JSONArray do
      begin
        Balance_Asset := JSONValue.GetValue<string>('asset');
        Balance_Free := JSONValue.GetValue<string>('free');
        Balance_Locked := JSONValue.GetValue<string>('locked');

        if Symbol = Balance_Asset then
        begin
          if not CalcLocked then
            AccountBalance := StrToFloat(Balance_Free)
          else
          begin
            AccountBalance := StrToFloat(Balance_Free) - StrToFloat(Balance_Locked);
            if AccountBalance < 0  then AccountBalance := 0;
          end;
          Result := True;
          Break;
        end;
      end;
    finally
      JSONValue.Free;
    end;
  except

  end;
end;

function TBinanceAPI.GetSymbolMinMaxPriceLast(NumOfLastCandle:Integer = 10;Symbol:String = 'BTCUSDT';CandleInterval:TCandleInterval = C1h):TMinMaxResult;
var CandleResultArray:TCandleResultArray;
    I,Counter:Integer;
    MidPrice:Real;
begin
  Result.ISReady := False;
  Result.Min := 0;
  Result.Mid := 0;
  Result.Max := 0;
  Result.HGreen := 0;
  Result.CGreen := 0;
  Result.CRed := 0;
  Result.LastUpdate := 0;
  try
    try
      CandleResultArray := GetSymbolKLines(Symbol,CandleInterval,NumOfLastCandle);
      if CandleResultArray <> Nil then
      begin
        Counter := 0;
        for I := High(CandleResultArray) downto Low(CandleResultArray) do
        begin
          MidPrice := (CandleResultArray[I].High + CandleResultArray[I].Low) / 2;
          Result.Mid := Result.Mid + MidPrice;
          if CandleResultArray[I].Open > CandleResultArray[I].Close then
          begin
            Inc(Result.CRed,I);
          end;
          if CandleResultArray[I].Open < CandleResultArray[I].Close then
          begin
            Inc(Result.CGreen,I);
          end;

          if Result.Min = 0 Then Result.Min := CandleResultArray[I].Low;
          if CandleResultArray[I].Low < Result.Min Then Result.Min := CandleResultArray[I].Low;

          if Result.Max = 0 Then Result.Max := CandleResultArray[I].High;
          if CandleResultArray[I].High > Result.Max Then Result.Max := CandleResultArray[I].High;

          Inc(Counter);
          if Counter = NumOfLastCandle then Break;
        end;
        Result.Mid := Result.Mid / NumOfLastCandle;
        Result.LastUpdate := Now;
        Result.ISReady := True;
        Result.HGreen := ((100 * Result.CGreen) Div (Result.CRed + Result.CGreen));
      end;
    finally
    end;
  except

  end;
end;


function TBinanceAPI.GetSymbolKLines(Symbol:String = 'BTCUSDT';CandleInterval:TCandleInterval = C1h;Limit:Integer = 30):TCandleResultArray;
var HttpHeaders,HttpResult:String;
    JSONValue:TJSONValue;
    JSONObject:TJSONObject;
    APIQuery:String;
    KLineList,KLine:TJSONArray;
    KLineValue:TJsonValue;
    I:Integer;
    A:String;
begin
  HttpResult := '';
  Result := Nil;
  try
    APIQuery := 'symbol='+Symbol+'&interval='+Copy(GetEnumName(System.TypeInfo(TCandleInterval),Ord(CandleInterval)),2,255)+'&limit='+Limit.ToString;
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/klines?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        KLineList := JSONObject.ParseJSONValue(HttpResult) as TJSONArray;
        I:= 0;
        if KLineList <> Nil then
        begin
          SetLength(Result,KLineList.Count);
          for KLineValue in KLineList do
          begin
            KLine := KLineValue.GetValue<TJSONArray>();
            Result[I].OpenTime := UnixToDateTime(KLine.Items[0].GetValue<Int64> div 1000);
            Result[I].Open := KLine.Items[1].GetValue<real>;
            Result[I].High := KLine.Items[2].GetValue<real>;
            Result[I].Low := KLine.Items[3].GetValue<real>;
            Result[I].Close := KLine.Items[4].GetValue<real>;
            Result[I].Volume := KLine.Items[5].GetValue<real>;
            Result[I].CloseTime := UnixToDateTime(KLine.Items[6].GetValue<Int64> div 1000);
            Result[I].Quote := KLine.Items[7].GetValue<real>;
            Result[I].TotalTrade := KLine.Items[8].GetValue<Int64>;
            Result[I].TakerBuyBaseVolume := KLine.Items[9].GetValue<real>;
            Result[I].TakerBuyQuoteVolume := KLine.Items[10].GetValue<real>;
            Result[I].Ignore := KLine.Items[11].GetValue<real>;
            Inc(I);
          end;
        end;
      finally
        KLineList.Free;
      end;
    end;
  except
    Result := Nil;
  end;
end;

function TBinanceAPI.GetSymbolAveragePrice(Symbol:String = 'BTCUSDT'):real;
var HttpHeaders,HttpResult:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    TradFeeList:TJSONArray;
begin
  HttpResult := '';
  Result := 0;
  try
    APIQuery := 'symbol='+Symbol;
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/avgPrice?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        JSONValue := TJSonObject.ParseJSONValue(HttpResult);
        Result := JSONValue.GetValue<real>('price');
      finally
        JSONValue.Free;
      end;
    end;
  except
    Result := 0;
  end;
end;

function TBinanceAPI.GetTradeFee(Symbol:String = 'BTCUSDT'):TTradFee;
var HttpHeaders,HttpResult:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    TradFeeList:TJSONArray;
begin
  HttpResult := '';
  Result.Symbol := '';
  Result.Maker := 0;
  Result.Taker := 0;
  try
    APIQuery := '&symbol='+Symbol;
    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/wapi/v3/tradeFee.html?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        JSONValue := TJSonObject.ParseJSONValue(HttpResult);
        Result.Symbol := JSONValue.GetValue<string>('tradeFee[0].symbol');
        Result.Maker := JSONValue.GetValue<real>('tradeFee[0].maker');
        Result.Taker := JSONValue.GetValue<real>('tradeFee[0].taker');
      finally
        JSONValue.Free;
      end;
    end;
  except
    Result.Symbol := '';
    Result.Maker := 0;
    Result.Taker := 0;
  end;
end;

function TBinanceAPI.TradeAllOrder(Symbol:String = 'BTCUSDT'):TOrderListResultArray;
var HttpResult,HttpHeaders:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    OrderList:TOrderListResult;
    JSONObject:TJSONObject;
    OrdersList:TJSONArray;
    OrderValue:TJsonValue;
    I:Integer;
begin
  HttpResult := '';
  Result := Nil;
  try
    APIQuery := '&symbol='+Symbol;

    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/allOrders?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        OrdersList := JSONObject.ParseJSONValue(HttpResult) as TJSONArray;
        I:= 0;
        if OrdersList <> Nil then
        begin
          SetLength(Result,OrdersList.Count);
          for OrderValue in OrdersList do
          begin
            Result[I].Symbol := OrderValue.GetValue<string>('symbol');
            Result[I].OrderId := OrderValue.GetValue<Int64>('orderId');
            Result[I].clientOrderId := OrderValue.GetValue<string>('clientOrderId');
            Result[I].Price := OrderValue.GetValue<real>('price');
            Result[I].OrigQty := OrderValue.GetValue<real>('origQty');
            Result[I].OrderStatus := TOrderStatus(GetEnumValue(TypeInfo(TOrderStatus),OrderValue.GetValue<string>('status')));
            Result[I].OrderType := TOrderType(GetEnumValue(TypeInfo(TOrderType),OrderValue.GetValue<string>('type')));
            Result[I].OrderSide := TOrderSide(GetEnumValue(TypeInfo(TOrderSide),OrderValue.GetValue<string>('side')));
            Result[I].StopPrice := OrderValue.GetValue<real>('stopPrice');
            Result[I].OrderTime := UnixToDateTime(OrderValue.GetValue<Int64>('time') div 1000);
            Result[I].IsWorking := Boolean(GetEnumValue(TypeInfo(Boolean),OrderValue.GetValue<string>('isWorking')));
            Inc(I);
          end;
        end;
      finally
        OrdersList.Free;
      end;
    end;
  except
    Result := Nil;
  end;
end;

function TBinanceAPI.TradeCurrentOpenOrders(Symbol:String = 'BTCUSDT'):TOrderListResultArray;
var HttpResult,HttpHeaders:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    OrderList:TOrderListResult;
    JSONObject:TJSONObject;
    OrdersList:TJSONArray;
    OrderValue:TJsonValue;
    I:Integer;
begin
  HttpResult := '';
  Result := Nil;
  try
    APIQuery := '&symbol='+Symbol;
    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/openOrders?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        OrdersList := JSONObject.ParseJSONValue(HttpResult) as TJSONArray;
        I:= 0;
        if OrdersList <> Nil then
        begin
          SetLength(Result,OrdersList.Count);
          for OrderValue in OrdersList do
          begin
            Result[I].Symbol := OrderValue.GetValue<string>('symbol');
            Result[I].OrderId := OrderValue.GetValue<Int64>('orderId');
            Result[I].clientOrderId := OrderValue.GetValue<string>('clientOrderId');
            Result[I].Price := OrderValue.GetValue<real>('price');
            Result[I].OrigQty := OrderValue.GetValue<real>('origQty');
            Result[I].OrderStatus := TOrderStatus(GetEnumValue(TypeInfo(TOrderStatus),OrderValue.GetValue<string>('status')));
            Result[I].OrderType := TOrderType(GetEnumValue(TypeInfo(TOrderType),OrderValue.GetValue<string>('type')));
            Result[I].OrderSide := TOrderSide(GetEnumValue(TypeInfo(TOrderSide),OrderValue.GetValue<string>('side')));
            Result[I].StopPrice := OrderValue.GetValue<real>('stopPrice');
            Result[I].OrderTime := UnixToDateTime(OrderValue.GetValue<Int64>('time') div 1000);
            Result[I].IsWorking := Boolean(GetEnumValue(TypeInfo(Boolean),OrderValue.GetValue<string>('isWorking')));
            Inc(I);
          end;
        end;
      finally
        OrdersList.Free;
      end;
    end;
  except
    Result := Nil;
  end;
end;

function TBinanceAPI.TradeCancelOrder(QueryOrderParams:TQueryOrderParams;var OrderListResult:TOrderListResult):Boolean;
var HttpResult,HttpHeaders:String;
    APIQuery:String;
    JSONObject:TJSONObject;
    JSONValue:TJSONValue;
    MessageCode,MessageDesc,ResData:String;
begin
  HttpResult := '';
  Result := False;
  try
    APIQuery := '&symbol='+QueryOrderParams.Symbol;

    APIQuery := APIQuery + '&orderId='+IntToStr(QueryOrderParams.OrderId);

    if Length(QueryOrderParams.origClientOrderId) > 0 then
      APIQuery := APIQuery + '&origClientOrderId ='+QueryOrderParams.origClientOrderId;

    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/order?'+APIQuery,HttpHeaders,HttpResult,cmDelete,FCustomHeaders) then
    begin
      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        JSONValue := JSONObject.ParseJSONValue(HttpResult) as TJSONValue;

        if JSONValue <> Nil then
        begin
          if JSONValue.TryGetValue('code',ResData) Then
          begin
            MessageCode := JSONValue.GetValue<string>('code');
            MessageDesc := JSONValue.GetValue<string>('msg');
          end;
          if JSONValue.TryGetValue('symbol',ResData) Then
          begin
            OrderListResult.Symbol := JSONValue.GetValue<string>('symbol');
            OrderListResult.OrderId := JSONValue.GetValue<Int64>('orderId');
            OrderListResult.clientOrderId := JSONValue.GetValue<string>('clientOrderId');
            OrderListResult.Price := JSONValue.GetValue<real>('price');
            OrderListResult.OrigQty := JSONValue.GetValue<real>('origQty');
            OrderListResult.OrderStatus := TOrderStatus(GetEnumValue(TypeInfo(TOrderStatus),JSONValue.GetValue<string>('status')));
            OrderListResult.OrderType := TOrderType(GetEnumValue(TypeInfo(TOrderType),JSONValue.GetValue<string>('type')));
            OrderListResult.OrderSide := TOrderSide(GetEnumValue(TypeInfo(TOrderSide),JSONValue.GetValue<string>('side')));

            if OrderListResult.OrderStatus = CANCELED then Result := True;
          end;
        end;
      finally
        JSONValue.Free;
      end;
    end;
  except
    Result := False;
  end;
end;

function TBinanceAPI.TradeQueryOrder(QueryOrderParams:TQueryOrderParams;var OrderListResult:TOrderListResult):Boolean;
var HttpResult,HttpHeaders:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    JSONObject:TJSONObject;
    MessageCode,MessageDesc,ResData:String;
    OrderStatus:TOrderStatus;
begin
  HttpResult := '';
  Result := False;
  try
     APIQuery := '&symbol='+QueryOrderParams.Symbol;

     APIQuery := APIQuery + '&orderId='+IntToStr(QueryOrderParams.OrderId);

    if Length(QueryOrderParams.origClientOrderId) > 0 then
      APIQuery := APIQuery + '&origClientOrderId ='+QueryOrderParams.origClientOrderId;

    APIQuery := GetSignedQuery(APIQuery);
    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/order?'+APIQuery,HttpHeaders,HttpResult,cmGet,FCustomHeaders) then
    begin
      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        JSONValue := JSONObject.ParseJSONValue(HttpResult) as TJSONValue;

        if JSONValue <> Nil then
        begin
          if JSONValue.TryGetValue('code',ResData) Then
          begin
            MessageCode := JSONValue.GetValue<string>('code');
            MessageDesc := JSONValue.GetValue<string>('msg');
          end;

          if JSONValue.TryGetValue('symbol',ResData) Then
          begin
            OrderListResult.Symbol := JSONValue.GetValue<string>('symbol');
            OrderListResult.OrderId := JSONValue.GetValue<Int64>('orderId');
            OrderListResult.clientOrderId := JSONValue.GetValue<string>('clientOrderId');
            OrderListResult.Price := JSONValue.GetValue<real>('price');
            OrderListResult.OrigQty := JSONValue.GetValue<real>('origQty');
            OrderListResult.OrderStatus := TOrderStatus(GetEnumValue(TypeInfo(TOrderStatus),JSONValue.GetValue<string>('status')));
            OrderListResult.OrderType := TOrderType(GetEnumValue(TypeInfo(TOrderType),JSONValue.GetValue<string>('type')));
            OrderListResult.OrderSide := TOrderSide(GetEnumValue(TypeInfo(TOrderSide),JSONValue.GetValue<string>('side')));
            OrderListResult.StopPrice := JSONValue.GetValue<real>('stopPrice');
            OrderListResult.OrderTime := UnixToDateTime(JSONValue.GetValue<Int64>('time') div 1000);
            OrderListResult.IsWorking := Boolean(GetEnumValue(TypeInfo(Boolean),JSONValue.GetValue<string>('isWorking')));
            Result := True;
          end;
        end;
      finally
        JSONValue.Free;
      end;
    end;
  except

  end;
end;

function TBinanceAPI.TradeNewOrder(var HttpResult:String;var NewOrderResult:TNewOrderResult;OrderParams:TNewOrderParams):Boolean;
var HttpHeaders:String;
    JSONValue:TJSONValue;
    APIQuery:String;
    TradFeeList:TJSONArray;
    JSONObject:TJSONObject;
    JSONArray:TJSONArray;
begin
  HttpResult := '{}';
  Result := False;
  try
    APIQuery := '&symbol='+OrderParams.Symbol;
    APIQuery := APIQuery + '&side='+GetEnumName(System.TypeInfo(TOrderSide),Ord(OrderParams.OrderSide));
    APIQuery := APIQuery + '&type='+GetEnumName(System.TypeInfo(TOrderType),Ord(OrderParams.OrderType));

    if Length(OrderParams.NewClientOrderId) > 0 then
      APIQuery := APIQuery + '&newClientOrderId ='+OrderParams.NewClientOrderId;

    case OrderParams.OrderType of
      LIMIT:
        begin
          APIQuery := APIQuery + '&timeInForce='+GetEnumName(System.TypeInfo(TTimeInForce),Ord(OrderParams.TimeInForce));
          APIQuery := APIQuery + '&quantity='+FloatToStrFormated(OrderParams.Quantity);
          APIQuery := APIQuery + '&price='+FloatToStrF(OrderParams.Price,ffGeneral,10,0);
        end;
      MARKET,TAKE_PROFIT:
        begin
          if OrderParams.Quantity > 0 then
            APIQuery := APIQuery + '&quantity='+FloatToStrFormated(OrderParams.Quantity)
          else
            APIQuery := APIQuery + '&quoteOrderQty='+FloatToStrFormated(OrderParams.QuoteOrderQty);
        end;
      STOP_LOSS:
        begin
          APIQuery := APIQuery + '&quantity='+FloatToStrFormated(OrderParams.Quantity);
          APIQuery := APIQuery + '&stopPrice='+FloatToStrFormated(OrderParams.StopPrice);
        end;
      STOP_LOSS_LIMIT,TAKE_PROFIT_LIMIT:
        begin
          APIQuery := APIQuery + '&timeInForce='+GetEnumName(System.TypeInfo(TTimeInForce),Ord(OrderParams.TimeInForce));
          APIQuery := APIQuery + '&quantity='+FloatToStrFormated(OrderParams.Quantity);
          APIQuery := APIQuery + '&price='+FloatToStrFormated(OrderParams.Price);
          APIQuery := APIQuery + '&stopPrice='+FloatToStrFormated(OrderParams.StopPrice);
        end;
      LIMIT_MAKER:
        begin
          APIQuery := APIQuery + '&quantity='+FloatToStrFormated(OrderParams.Quantity);
          APIQuery := APIQuery + '&price='+FloatToStrFormated(OrderParams.Price);
        end;
    end;
    APIQuery := GetSignedQuery(APIQuery);

    if FRestAPI.TCPConnect(FAPIUrlAddress+'/api/v3/order?'+APIQuery,HttpHeaders,HttpResult,cmPost,FCustomHeaders) then
    begin
      Result := CheckHttpOK(HttpHeaders);

      try
        HttpResult := ReplaceAll(HttpResult,#13,'');
        HttpResult := ReplaceAll(HttpResult,#10,'');
        JSONValue := JSONObject.ParseJSONValue(HttpResult) as TJSONValue;

        if JSONValue <> Nil then
        begin
          NewOrderResult.Symbol := JSONValue.GetValue<string>('symbol');
          NewOrderResult.OrderId := JSONValue.GetValue<Int64>('orderId');
          NewOrderResult.clientOrderId := JSONValue.GetValue<string>('clientOrderId');
          NewOrderResult.TransactTime := UnixToDateTime(JSONValue.GetValue<Int64>('transactTime') div 1000);
          NewOrderResult.Price := JSONValue.GetValue<real>('price');
          NewOrderResult.OrigQty := JSONValue.GetValue<real>('origQty');
          NewOrderResult.ExecutedQty := JSONValue.GetValue<real>('executedQty');
          NewOrderResult.CummulativeQuoteQty := JSONValue.GetValue<real>('cummulativeQuoteQty');
          NewOrderResult.OrderStatus := TOrderStatus(GetEnumValue(TypeInfo(TOrderStatus),JSONValue.GetValue<string>('status')));
          NewOrderResult.OrderType := TOrderType(GetEnumValue(TypeInfo(TOrderType),JSONValue.GetValue<string>('type')));
          NewOrderResult.OrderSide := TOrderSide(GetEnumValue(TypeInfo(TOrderSide),JSONValue.GetValue<string>('side')));

          JSONArray := JSONValue.GetValue<TJSONArray>('fills');
          for JSONValue in JSONArray do
          begin
            NewOrderResult.FillPrice := JSONValue.GetValue<real>('price');
            NewOrderResult.FillQuantity := JSONValue.GetValue<real>('qty');
            NewOrderResult.FillCommission := JSONValue.GetValue<real>('commission');
          end;
        end;
      finally
        JSONValue.Free;
      end;

    end;
  except
    Result := false;
  end;
end;

end.
