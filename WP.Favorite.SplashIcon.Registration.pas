{
  Unit Name   : WP.Favorite.SplashIcon.Registration.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description : Register the icon on the splash screen

  History:
    -

  Notes:
}

unit WP.Favorite.SplashIcon.Registration;

interface

uses
  Winapi.Windows;

var
  bmSplashScreen: HBITMAP;

const
  VERSION = 'Ver 1.0.1';

implementation

uses
  ToolsAPI, SysUtils, Vcl.Dialogs;

resourcestring
  resPackageName = 'Cevahir Favorite ' + VERSION;
  resLicense = 'Freemium License';
  resAboutTitle = '';
  resAboutDescription = '';

initialization
  bmSplashScreen := LoadBitmap(hInstance, 'SPLASH');
  (SplashScreenServices as IOTASplashScreenServices).AddPluginBitmap(resPackageName, bmSplashScreen, False, resLicense);

end.
