unit notesrv;

{$mode objfpc}{$H+}

interface

// If you are using FPC only or want to manually add PostgreSQL support, add pqconnection to your uses clause...

uses
  Classes, SysUtils, httpdefs, httproute, fpjson, jsonparser,
  sqldb, ibconnection, pqconnection,
  Dos,
  server;

type

  { TNotesAppHandler }

  TNotesAppHandler = class(TAppHandler)
  private
    FConnection: TPQConnection;
    FTransaction: TSQLTransaction;
    function LoadData(): TJSONObject;
	  function LoadDatax(): TJSONObject;
    function RecordExists(table, field, value: string): boolean;
    procedure DeleteRecord(table, field, value: string);
    procedure UpdateRecord(table, idField, idValue: string; data: TJSONObject);
    procedure SaveData(AData: TJSONObject);
    procedure InitializeConnection;
  public
    constructor Create(ARootPath: string);
    destructor Destroy; override;

    procedure notesApi(req: TRequest; res: TResponse);
  end;

implementation


{ TNotesAppHandler }
constructor TNotesAppHandler.Create(ARootPath: string);
begin
  inherited Create(ARootPath);

  // Initialize database connection and transaction
  FConnection := TPQConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(FConnection);

  // Set up connection properties
  FConnection.Transaction := FTransaction;
  FConnection.UserName := GetEnv('PG_USER');
  FConnection.Password := GetEnv('PG_PASS');
  FConnection.DatabaseName := GetEnv('PG_DB');
  FConnection.HostName := GetEnv('PG_HOST');
  FConnection.Params.Add('port=' + GetEnv('PG_PORT'));
end;

destructor TNotesAppHandler.Destroy;
begin
  if FConnection.Connected then
    FConnection.Close;

  FTransaction.Free;
  FConnection.Free;

  inherited Destroy;
end;

procedure TNotesAppHandler.InitializeConnection;
begin
  FConnection.UserName := GetEnv('PG_USER');
  FConnection.Password := GetEnv('PG_PASS');
  FConnection.DatabaseName := GetEnv('PG_DB');
  FConnection.HostName := GetEnv('PG_HOST');
  FConnection.Params.Add('port=' + GetEnv('PG_PORT')); // Explicitly set port if needed
end;

function TNotesAppHandler.LoadDatax(): TJSONObject;
var
  C: TPQConnection;
  T: TSQLTransaction;
  Q: TSQLQuery;
  jusers: TJSONArray;
begin
  Result := nil;
  C := nil;
  T := nil;
  Q := nil;
  jusers := TJSONArray.Create;
  
  try
    // Initialize database connection
    C := TPQConnection.Create(nil);
    T := TSQLTransaction.Create(C);
    Q := TSQLQuery.Create(C);

    // Setup connection credentials and transaction
    C.UserName := GetEnv('PG_USER');
    C.Password := GetEnv('PG_PASS');
    C.DatabaseName := GetEnv('PG_DB');
    C.HostName := GetEnv('PG_HOST');
    C.Params.Add('port=' + GetEnv('PG_PORT'));
    C.Transaction := T;

    // Connect to the database
    try
      C.Connected := True;
    except
      on E: Exception do
        raise Exception.Create('Error connecting to database: ' + E.Message);
    end;

    // Setup query
    Q.Database := C;
    Q.Transaction := T;
    Q.SQL.Text := 'SELECT id, email FROM USERS';

    // Execute query
    try
      Q.Open;
    except
      on E: Exception do
        raise Exception.Create('Error executing query: ' + E.Message);
    end;

    // Process results
    try
      while not Q.EOF do
      begin
        jusers.Add(TJSONObject.Create([
          'id', Q.FieldByName('id').AsString,
          'email', Q.FieldByName('email').AsString
        ]));
        Q.Next;
      end;
    except
      on E: Exception do
        raise Exception.Create('Error processing query results: ' + E.Message);
    end;

    // Prepare JSON result
    Result := TJSONObject.Create(['users', jusers]);
  finally
    // Cleanup resources
    if Assigned(Q) then Q.Free;
    if Assigned(T) then T.Free;
    if Assigned(C) then
    begin
      if C.Connected then
        C.Connected := False;
      C.Free;
    end;

    // If something went wrong, ensure jusers is freed
    if not Assigned(Result) then
      jusers.Free;
  end;
end;

function TNotesAppHandler.LoadData(): TJSONObject;
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create();
  FConnection.Open;
  try
    ms.LoadFromFile(FRootPath + FDataFile);
    // Result := TJSONObject(GetJSON(ms, False));
  finally
	writeln('huh?');
	writeln(FRootPath + FDataFile);
	Result := TJSONObject(GetJSON('{"Fld1" : "Hello", "Fld2" : 42, "Colors" : ["Red", "Green", "Blue"]}'));
    ms.Free;
    FConnection.Close;
  end;
end;

procedure TNotesAppHandler.SaveData(AData: TJSONObject);
var
  Q: TSQLQuery;
  idField: Integer;
  emailField: string;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.Transaction := FConnection.Transaction;

    // Extract data from JSON
    idField := AData.Integers['id'];
    emailField := AData.Strings['email'];

    // Prepare and execute the SQL query
    Q.SQL.Text := 'INSERT INTO users (id, email) VALUES (:id, :email) ' +
                  'ON CONFLICT (id) DO UPDATE SET email = :email;';
    Q.Params.ParamByName('id').AsInteger := idField;
    Q.Params.ParamByName('email').AsString := emailField;

    try
      FConnection.Open; // Ensure the connection is open
      Q.ExecSQL;
      FConnection.Transaction.Commit; // Commit the transaction
    except
      on E: Exception do
      begin
        FConnection.Transaction.Rollback; // Rollback on error
        raise Exception.Create('Error saving data to database: ' + E.Message);
      end;
    end;
  finally
    Q.Free;
    FConnection.Close; // Close the connection
  end;
end;

function TNotesAppHandler.RecordExists(table, field, value: string): boolean;
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.SQL.Text := Format('SELECT 1 FROM %s WHERE %s = :value LIMIT 1', [table, field]);
    Q.Params.ParamByName('value').AsString := value;
    Q.Open;
    Result := not Q.EOF;
  finally
    Q.Free;
  end;
end;

procedure TNotesAppHandler.DeleteRecord(table, field, value: string);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.SQL.Text := Format('DELETE FROM %s WHERE %s = :value', [table, field]);
    Q.Params.ParamByName('value').AsString := value;
    Q.ExecSQL;
    FConnection.Transaction.Commit;
  except
    FConnection.Transaction.Rollback;
    raise;
  end;
end;

procedure TNotesAppHandler.UpdateRecord(table, idField, idValue: string; data: TJSONObject);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.Database := FConnection;
    Q.SQL.Text := Format('UPDATE %s SET email = :email WHERE %s = :id::INTEGER', [table, idField]);
    Q.Params.ParamByName('email').AsString := data.Strings['email']; // Get 'email' from JSON
    Q.Params.ParamByName('id').AsInteger := StrToInt(idValue); // Convert idValue to integer
    try
      Q.ExecSQL;
      FTransaction.Commit; // Commit transaction
    except
      on E: Exception do
      begin
        FTransaction.Rollback; // Rollback transaction on error
        raise Exception.Create('Error updating record: ' + E.Message); // Raise detailed error
      end;
    end;
  finally
    Q.Free; // Clean up query object
  end;
end;

procedure TNotesAppHandler.notesApi(req: TRequest; res: TResponse);
var
  id: string;
  jres, jparam: TJSONObject;
  found: boolean;
begin
  if CompareText(req.Method, 'GET') = 0 then
  begin
    // Handle GET: Retrieve all users
    try
      jres := LoadDatax(); // Load data from the database
      jsonResponse(res, jres);
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error loading data"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'POST') = 0 then
  begin
    // Handle POST: Create new user
    try
      jparam := TJSONObject(GetJSON(req.Content, False));
      try
        SaveData(jparam); // Save new user to the database
        res.Code := 201; // Created
        jsonResponse(res, TJSONObject.Create(['message', 'User created']));
      finally
        jparam.Free;
      end;
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error creating user"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'PUT') = 0 then
  begin
    // Handle PUT: Update existing user
    id := req.RouteParams['id'];
    try
      if not RecordExists('users', 'id', id) then
        begin
          res.Code := 404; // Not Found
          res.Content := '{"error":"User not found"}';
          res.SendContent;
          Exit;
        end;
      // Parse incoming JSON data
      jparam := TJSONObject(GetJSON(req.Content, False));
      try
        // Update the user in the database
        UpdateRecord('users', 'id', id, jparam);
        jsonResponse(res, TJSONObject.Create(['message', 'User updated']));
      finally
        jparam.Free;
      end;
    except
      on E: Exception do
      begin
        writeln('Error updating user: ' + E.Message);
        res.Code := 500; // Internal Server Error
        res.Content := '{"error":"Error updating user"}';
        res.SendContent;
      end;
    end;
  end
  else if CompareText(req.Method, 'DELETE') = 0 then
  begin
    // Handle DELETE: Delete a user
    id := req.RouteParams['id'];
    try
      // Check if the user exists
      if not RecordExists('users', 'id', id) then
      begin
        res.Code := 404; // Not Found
        res.Content := '{"error":"User not found"}';
        res.SendContent;
        Exit;
      end;

      // Delete the user from the database
      DeleteRecord('users', 'id', id);
      jsonResponse(res, TJSONObject.Create(['message', 'User deleted']));
    except
      on E: Exception do
      begin
        writeln('Error: ' + E.Message);
        res.Code := 500;
        res.Content := '{"error":"Error deleting user"}';
        res.SendContent;
      end;
    end;
  end
  else
  begin
    res.Code := 405; // Method Not Allowed
    res.Content := '{"error":"Method not allowed"}';
    res.SendContent;
  end;
end;

end.
