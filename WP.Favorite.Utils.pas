{
  Unit Name   : WP.Favorite.Utils.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description : Tools required for extracting or
                registering information in the Windows registry

  History:
    -

  Notes:
}

unit WP.Favorite.Utils;

interface

uses
  System.SysUtils
  , System.Classes
  , System.StrUtils
  , System.Win.Registry
  , Winapi.Windows
  ;

Type
  TFavItem = class
    FavoriteIndex: Integer;
    FavoriteID: string;
    FavoriteName: string;
    constructor Create(const AFavoriteIndex: Integer; AFavoriteID, AFavoriteName: string);
  end;

  function GetFavoriteList(var aFavoritelist: TStringList): boolean;
  function LoadFromFavorite(Favorite: String; var Project: TStringList): boolean;
  function SaveFavorite(FavoriteName: String): Integer;
  procedure EditFavorite(FavoriteId , FavoriteName: String);
  procedure UpdateFavorite(FavoriteId , SortId , FavoriteName: String);
  procedure SaveToFavorite(Favorite: String; ProjectName: TStrings);
  procedure DeleteFavorite(Favorite: String);
  function GetRecentProjects(RecentProjects : TStrings) : Boolean;
  function CompareFavorites(List: TStringList; Index1, Index2: Integer): Integer;

implementation


function GetRecentProjects(RecentProjects : TStrings) : Boolean;
var
  reg : TRegistry;
begin
  RecentProjects.Clear;
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CURRENT_USER;
    if reg.KeyExists('\SOFTWARE\Embarcadero\BDS\23.0\Closed Projects') then
    begin
      reg.OpenKey('\SOFTWARE\Embarcadero\BDS\23.0\Closed Projects',False);
      var maxprj:=reg.ReadInteger('Max Closed Files');
      for var I := 0 to maxprj-1 do
      begin
        if reg.ValueExists('File_'+inttostr(I)) then
        begin
          var filedata:=reg.ReadString('File_'+inttostr(I));
          var filedataS:=filedata.Split([',']);
          RecentProjects.Add(Copy(filedataS[1],2,Length(filedataS[1])-2));
        end;
      end;
    end;
  finally
    reg.Free;
  end;
end;

function GetFavoriteList(Var aFavoritelist : TStringList) : boolean;
var
  reg : TRegistry;
  FavoriteID : TStringList;
begin
  Result:=False;
  reg:=TRegistry.Create;
  FavoriteID:=TStringList.Create;
  try
    reg.RootKey:=HKEY_CURRENT_USER;
    if reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False) then
    begin
      Reg.GetKeyNames(FavoriteID);
      for var i := 0 to FavoriteID.Count - 1 do
      begin
        var FavValue:=reg.ReadString(FavoriteID[i]);
        var Fav:=SplitString(FavValue,',');
        aFavoritelist.AddObject(Fav[0],TFavItem.Create(StrToInt(Fav[0]),FavoriteID[i],Fav[1]));
      end;
      if aFavoritelist.Count>0 then
        Result:=True;
    end;
    reg.CloseKey();
  finally
    reg.Free;
    FavoriteID.Free;
  end;
end;

function LoadFromFavorite(Favorite: String; var Project: TStringList): boolean;
var
  reg: TRegistry;
  values: TStringList;
  i: Integer;
begin
  Result := False;
  reg := TRegistry.Create;
  values := TStringList.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('Software\CevahirSoft\Delphi\Favorite\' + Favorite, False) then
    begin
      reg.GetValueNames(values);
      for i := 0 to values.Count - 1 do
        Project.Add(reg.ReadString(values[i]));
      Result := Project.Count > 0;
    end;
  finally
    reg.Free;
    values.Free;
  end;
end;

function SaveFavorite(FavoriteName: String): Integer;
var
  reg : TRegistry;
begin
  Result:=-1;
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CURRENT_USER;
    if not reg.KeyExists('Software\CevahirSoft\Delphi\Favorite') then
    begin
      reg.CreateKey('Software\CevahirSoft\Delphi\Favorite');
      reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False);
      reg.WriteInteger('Count',0);
    end
    else
      reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False);
    Result:=reg.ReadInteger('Count');
    Result:=Result+1;
    reg.WriteInteger('Count',Result);
    reg.WriteString(Result.ToString,Result.ToString+','+FavoriteName);
    reg.CloseKey();
  finally
    reg.Free;
  end;
end;

procedure EditFavorite(FavoriteId , FavoriteName: String);
var
  reg : TRegistry;
begin
  reg:=TRegistry.Create;
  reg.RootKey:=HKEY_CURRENT_USER;
  reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False);
  var Favarite:=reg.ReadString(FavoriteId);
  var st:=SplitString(Favarite,',');
  reg.WriteString(FavoriteId,st[0]+','+FavoriteName);
  reg.CloseKey();
  reg.Free;
end;

procedure UpdateFavorite(FavoriteId , SortId , FavoriteName: String);
var
  reg : TRegistry;
begin
  reg:=TRegistry.Create;
  reg.RootKey:=HKEY_CURRENT_USER;
  reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False);
  reg.WriteString(FavoriteId,SortId+','+FavoriteName);
  reg.CloseKey();
  reg.Free;
end;

procedure SaveToFavorite(Favorite: String; ProjectName : TStrings);
var
  reg : TRegistry;
begin
  reg:=TRegistry.Create;
  reg.RootKey:=HKEY_CURRENT_USER;
  if reg.KeyExists('Software\CevahirSoft\Delphi\Favorite\'+Favorite) then
    reg.DeleteKey('Software\CevahirSoft\Delphi\Favorite\'+Favorite);
  reg.OpenKey('Software\CevahirSoft\Delphi\Favorite\'+Favorite,True);
  for var I := 0 to ProjectName.Count-1 do
    reg.WriteString(I.ToString,ProjectName[I]);
  reg.CloseKey();
  reg.Free;
end;

procedure DeleteFavorite(Favorite: String);
var
  reg : TRegistry;
begin
  reg:=TRegistry.Create;
  reg.RootKey:=HKEY_CURRENT_USER;
  if reg.KeyExists('Software\CevahirSoft\Delphi\Favorite\'+Favorite) then
    reg.DeleteKey('Software\CevahirSoft\Delphi\Favorite\'+Favorite);
  reg.OpenKey('Software\CevahirSoft\Delphi\Favorite',False);
  reg.WriteInteger('Count',reg.ReadInteger('Count')-1);
  reg.DeleteValue(Favorite);
  reg.CloseKey();
  reg.Free;
end;

function CompareFavorites(List: TStringList; Index1, Index2: Integer): Integer;
var
  Item1, Item2: TFavItem;
begin
  Item1 := TFavItem(List.Objects[Index1]);
  Item2 := TFavItem(List.Objects[Index2]);

  Result := Item1.FavoriteIndex - Item2.FavoriteIndex;
end;

{ TFavItem }

constructor TFavItem.Create(const AFavoriteIndex: Integer; AFavoriteID, AFavoriteName: string);
begin
  FavoriteIndex:=AFavoriteIndex;
  FavoriteID:=AFavoriteID;
  FavoriteName:=AFavoriteName;
end;

end.
