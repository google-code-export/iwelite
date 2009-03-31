NOTE:
  Before installing, please be sure that you have all service packs, hotfix rollups etc for your Delphi installation before installing our components.

  Also, please note that this Elite Suite unified Install is compatible only with Intraweb 9.0.28 and higher.  If you would like to use our components with earlier releases of IW, then please select to download the packages individually.

Help:
  Our documentation and component help can be found at http://arcanatech.com/docs


Installation Instructions:

1. Open IntrawebVersion.inc and ensure that the directive relating to your IW version is correctly defined and that all others are commented.  For example, if you have IW version 9, it should look similar to this:

{.$DEFINE INTRAWEB51}
{.$DEFINE INTRAWEB60}
{.$DEFINE INTRAWEB70}
{.$DEFINE INTRAWEB72}
{.$DEFINE INTRAWEB80}
{$DEFINE INTRAWEB90}


2. Open EliteSuite_IWxx_Delphiyyyy.dpk where xx is the IW version and yyyy is the delphi version.

3. Compile the package

4. Open EliteSuite_IWxx_Delphiyyyy_dsn.dpk where xx is the IW version and yyyy is the delphi version.

5. Under project options, add a search path to the intraweb source directory.

6. Compile the package.

7. Install the package.