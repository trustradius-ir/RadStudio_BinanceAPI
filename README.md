# RadStudio Binance API

This API used to connect to Binance API and Binance WebSocket

## Binance WebSocket Usage


```bash
uses WebSocket,...;
```

## Example

```python
type
 TMainForm = class(TForm)
 ... 
 private
    procedure OnWebSocketEvent(SecWebSocketKey: string; EventMessage: string);
    procedure OnWebSocketData(SecWebSocketKey: string; ReceivedData: string);
 end;

procedure TMainForm.OnWebSocketEvent(SecWebSocketKey: string; EventMessage: string);
begin
//  AddEvent(clBlack,EventMessage);
end;

procedure TMainForm.OnWebSocketData(SecWebSocketKey: string; ReceivedData: string);
var JSONValue:TJSONValue;
    JSONPrice,JSONEventTime:String;
    CurrentPrice:Real;
    CurrentPriceDT:TDateTime;
begin
    if WebSocketBinance.SecWebSocketKey = SecWebSocketKey then
    begin

      try
        JSONValue := TJSonObject.ParseJSONValue(ReceivedData);
        JSONPrice := JSONValue.GetValue<string>('p');
        JSONEventTime := JSONValue.GetValue<string>('E');
        CurrentPrice := StrToFloat(JSONPrice);
        CurrentPriceDT := TTimeZone.Local.ToLocalTime(UnixToDateTime(StrtoInt64(JSONEventTime) Div 1000));
      finally
        JSONValue.Free;
      end;
   end;
end;

....

var  WebSocketBinance:TWebSocket;
begin
  WebSocketBinance := TWebSocket.Create('stream.binance.com', 9443, True);
  WebSocketBinance.OnWebSocketEvent := OnWebSocketEvent;
  WebSocketBinance.OnWebSocketData := OnWebSocketData;
  WebSocketBinance.Connect('/ws/'+LowerCase(Symbol)+'@trade');
end;

```

## BinanceAPI Usage
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

```bash
uses BinanceAPI,...;
```

## Example

```python
var BinanceAPI:TBinanceAPI;
    AccountBalance:Real;
begin
  BinanceAPI := TBinanceAPI.Create('ApiKey','SecretKey');
  if not BinanceAPI.GetAccountInfo(APIResult) Then
  begin
    //Can not Connect
    Abort;
  end;
  BinanceAPI.GetAccountBalance(AccountBalance,'USDT');
end;
```

## License
[MIT](https://choosealicense.com/licenses/mit/)