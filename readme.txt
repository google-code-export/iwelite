NOTE:
  Before installing, please be sure that you have all service packs, hotfix rollups etc for your Delphi installation before installing our components.

  Also, please note that this Elite Suite unified Install is compatible only with Intraweb 9.0.28 and higher.  If you would like to use our components with earlier releases of IW, then please select to download the packages individually.

Help:
  Our documentation and component help can be found at http://arcanatech.com/docs


Installation Instructions:

1. Open IntrawebVersion.inc and ensure that the directive relating to your IW version is correctly defined and that all others are commented.  For example, if you have IW version XII (12), it should look similar to this:

{.$DEFINE INTRAWEB51}
{.$DEFINE INTRAWEB60}
{.$DEFINE INTRAWEB70}
{.$DEFINE INTRAWEB72}
{.$DEFINE INTRAWEB80}
{.$DEFINE INTRAWEB90}
{.$DEFINE INTRAWEB100}
{.$DEFINE INTRAWEB110}
{$DEFINE INTRAWEB120}


2. Open EliteSuitePhoenix.dpk.

3. Compile the package

4. Open dclEliteSuitePhoenix.dpk.

5. Compile the package.

6. Install the package.