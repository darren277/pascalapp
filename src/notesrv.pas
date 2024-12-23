unit notesrv;

{$mode objfpc}{$H+}

interface

// If you are using FPC only or want to manually add PostgreSQL support, add pqconnection to your uses clause...

uses
  Classes, SysUtils, httpdefs, httproute, fpjson, jsonparser,
  sqldb, ibconnection, pqconnection,
  server;

type

  { TNotesAppHandler }

  TNotesAppHandler = class(TAppHandler)
  private
    function LoadData(): TJSONObject;
	function LoadDatax(): TJSONObject;
    procedure SaveData(AData: TJSONObject);
  public
    constructor Create(ARootPath: string);
    destructor Destroy; override;

    procedure notesApi(req: TRequest; res: TResponse);
  end;

implementation


{ TNotesAppHandler }

function TNotesAppHandler.LoadDatax(): TJSONObject;
var
  C : TSQLConnection;
  T : TSQLTransaction;
  Q : TSQLQuery;
  VariableContainingPort: string;
  jusers: TJSONArray;
begin
  // Create a connection.
  //C:=TIBConnection.Create(Nil);
  C:=TPQConnection.Create(Nil);
  writeln('hi');
  try
    // Set credentials.
    C.UserName:='myusername';
    C.Password:='mypassword';
    C.DatabaseName:='postgres';
	// C.DBName:='postgres';
	C.HostName:='172.18.0.21';
	//C.Host:='172.18.0.21';
	
	jusers := TJSONArray.Create;

	
	VariableContainingPort := '5432';
	
	C.Params.Add('port=' + VariableContainingPort);
	
    // Create a transaction.
	try
        T:=TSQLTransaction.Create(C);
    except
        on E: Exception do
        begin
            writeln('Error creating transaction: ' + E.Message);
        end;
    end;

    // Point to the database instance
    T.Database:=C;

    // Now we can open the database.
    try
        C.Connected:=True;
    except
        on E: Exception do
        begin
            writeln('Error connecting to database: ' + E.Message);
        end;
    end;

    // Create a query to return data
    try
        Q:=TSQLQuery.Create(C);
    except
        on E: Exception do
        begin
            writeln('Error creating query: ' + E.Message);
        end;
    end;

    // Point to database and transaction.
    Q.Database:=C;
    Q.Transaction:=T;

    // Set the SQL select statement
    try
        Q.SQL.Text:='SELECT * FROM USERS';
    except
        on E: Exception do
        begin
            writeln('Error setting SQL: ' + E.Message);
        end;
    end;

    // And now use the standard TDataset methods.
    Q.Open;
    While not Q.EOF do
      begin
        try
            Writeln(Q.FieldByName('email').AsString);
			jusers.Add(TJSONObject.Create(['id', Q.FieldByName('id').AsString, 'email', Q.FieldByName('email').AsString]));
        except
            on E: Exception do
            begin
                writeln('Error reading field: ' + E.Message);
            end;
        end;
      Q.Next
      end;
    Q.Close;
  finally
	writeln('hi again');
	// Result := TJSONObject(GetJSON('{"Fld1" : "Hello", "Fld2" : 42, "Colors" : ["Red", "Green", "Blue"]}'));
	Result := TJSONObject(GetJSON('{"users" : '+jusers.AsJSON+'}'));
    C.Free;
  end;
end;

function TNotesAppHandler.LoadData(): TJSONObject;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create();
  try
    ms.LoadFromFile(FRootPath + FDataFile);
    // Result := TJSONObject(GetJSON(ms, False));
  finally
	writeln('huh?');
	writeln(FRootPath + FDataFile);
	Result := TJSONObject(GetJSON('{"Fld1" : "Hello", "Fld2" : 42, "Colors" : ["Red", "Green", "Blue"]}'));
    ms.Free;
  end;
end;

procedure TNotesAppHandler.SaveData(AData: TJSONObject);
var
  s: RawByteString;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create();
  try
    s := AData.FormatJSON(DefaultFormat);
    ms.WriteBuffer(s[1], Length(s));
    ms.SaveToFile(FRootPath + FDataFile);
  finally
    ms.Free;
  end;
end;

constructor TNotesAppHandler.Create(ARootPath: string);
begin
  inherited Create(ARootPath);
end;

destructor TNotesAppHandler.Destroy;
begin
  inherited Destroy;
end;

procedure TNotesAppHandler.notesApi(req: TRequest; res: TResponse);
var
  id: string;
  i: integer;
  jres, jdata, jparam: TJSONObject;
  jnotes: TJSONArray;
  found: boolean;
begin
  If CompareText(req.Method, 'GET') = 0 then
  begin
    // get all notes and colors
    try
      // jres := LoadData();
	  jres := LoadDatax();
	  // jres.Add('data', jdata);
	  try
	    jsonResponse(res, jres);
	  except
	      on E: Exception do
            begin
                writeln('Error: ' + E.Message);
                res.Code := 500;
                //jres := TJSONObject(GetJSON('{"error" : "Error loading data"}'));
                //jsonResponse(res, jres);
            end;
        end;
    finally
      jres.Free;
    end;
  end
  else If CompareText(req.Method, 'POST') = 0 then
  begin
    // create new note
    jres := TJSONObject.Create();
    jparam := TJSONObject(GetJSON(req.Content, False));
    try
      // jdata := LoadData();
	  jdata := LoadDatax();
      jnotes := jdata.Arrays['posts'];
      jnotes.Add(jparam);
      SaveData(jdata);

      jsonResponse(res, jres);
    finally
      jdata.Free;
      jres.Free;
    end;
  end
  else If CompareText(req.Method,'PUT') = 0 then
  begin
    // update existing note
    id := req.RouteParams['id'];
    jres := TJSONObject.Create();
    jparam := TJSONObject(GetJSON(req.Content, False));
    try
      jdata := LoadData();
      jnotes := jdata.Arrays['posts'];
      found := false;
      for i := jnotes.Count - 1 downto 0 do
        if AnsiSameText(TJSONObject(jnotes[i]).Strings['id'], id) then
        begin
          found := true;
          jnotes.Delete(i);
          break;
        end;
      if found then
      begin
        jnotes.Add(jparam);
        SaveData(jdata);
      end;

      jsonResponse(res, jres);
    finally
      jdata.Free;
      jres.Free;
    end;
  end
  else If CompareText(req.Method,'DELETE') = 0 then
  begin
    // delete node
    id := req.RouteParams['id'];
    jres := TJSONObject.Create();
    try
      jdata := LoadData();
      jnotes := jdata.Arrays['posts'];
      for i := jnotes.Count - 1 downto 0 do
        if AnsiSameText(TJSONObject(jnotes[i]).Strings['id'], id) then
        begin
          jnotes.Delete(i);
          break;
        end;
      SaveData(jdata);

      jsonResponse(res, jres);
    finally
      jdata.Free;
      jres.Free;
    end;

  end
  else
  begin
    Res.Code := 405;
    Res.CodeText := 'Method not allowed';
    res.SendContent;
    Exit;
  end;

end;


end.
