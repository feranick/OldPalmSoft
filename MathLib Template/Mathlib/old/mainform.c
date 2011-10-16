//
// File:        mainform.c
// Author:      Matthew Bevan (mbevan@marginsoftware.com)
// Description: Contains all the code to deal with the main window.
// History:     Last updated: 2001/12/29
//
//  Version 1.0.0 - 2002/02/07 08:32p - mbevan
//   - initial construction and release
//

#include <PalmOS.h>
#include <DLServer.h>

#include "common.h"
#include "globals.h"
#include "resource.h"
#include "mainform.h"

#include "MathLib.h"

#define DBL_MIN_EXP -100
#define DBL_EPSILON 2.2204460492503131e-016
#define FLT_RADIX 2

typedef struct { int quot; int rem; } div_t;

div_t idiv ( int num, int denom ) {
  div_t r;
  
  r.quot = num / denom;
  r.rem = num % denom;

/*
 * The ANSI standard says that |r.quot| <= |n/d|, where
 * n/d is to be computed in infinite precision.  In other
 * words, we should always truncate the quotient towards
 * 0, never -infinity.
 *
 * Machine division and remainer may work either way when
 * one or both of n or d is negative.  If only one is
 * negative and r.quot has been truncated towards -inf,
 * r.rem will have the same sign as denom and the opposite
 * sign of num; if both are negative and r.quot has been
 * truncated towards -inf, r.rem will be positive (will
 * have the opposite sign of num).  These are considered
 * `wrong'.
 *
 * If both are num and denom are positive, r will always
 * be positive.
 *
 * This all boils down to:
 *if num >= 0, but r.rem < 0, we got the wrong answer.
 * In that case, to get the right answer, add 1 to r.quot and
 * subtract denom from r.rem.
*/

  if (num >= 0 && r.rem < 0) {
    r.quot++;
    r.rem -= denom;
  }

  return ( r );
}

char *df_itoa (int arg, char *str, int radix ) {
  char *ret = str;
  char *s;
  div_t r;
  
  if ( arg < 0 ) {
    *str++ = '-';
    arg = -arg;
  }
  
  s = str;
  r.quot = arg;
  
  do {
    r = idiv( r.quot, radix );
    r.rem += ( r.rem < 10 ) ? '0' : 'a'-10 ;
    *s++ = r.rem;
  } while ( r.quot );
  
  *s-- = '\0';
  
  while( str<s ) {
    char ch = *str;
    *str++ = *s;
    *s-- = ch;
  }
  
  return ret;
}

char *fftoa ( double d, char *str, int p, char type, int trunc ) {
/*
* Convert a floating-point number into a
* formatted 'f','e' or 'E' type string.
* (c) Edmond J.Breen, March 1995.
* where:  'p'  represents the precision
*                i.e. the number of digits
*                that will be placed after
*                the decimal point.
*                A precision of 0 suppresses
*                the decimal point.
*         'type' is either 'f', 'e' or 'E'.
*                'f' ->  [-]mmm.dddd
*                'e' ->  [-]m.dddde+xx
*                      | [-]m.dddde-xx
*                'E' ->  [-]m.ddddE+xx
*                      | [-]m.ddddE-xx
*         'trunc' if trunc != 0 then trailing
*                 zeros will be removed.
*                 i.e. 'g' or 'G' format.
*/
  double M;
  int i;
  int j;
  int width;
  int spt;
  int dec;
  int prec;
  static char *s;
  prec = p;

  i = 0;
  if (d < 0) str[0] = '-', s = &str[1], d = -d;
  else s = str;

  if (d >= 1) {
    /**collect integer part**/
    int k;
    int c;
    double D = floor(d);
    d -= D;
    do {
      j = fmod(D,10.0);
      s[i++] = j + '0';
      D -= j;
    }
  
    while( (D /= 10) >= 1 ) ; /**now reverse the string **/
      for( k = i, j = 0, k--  ;  j < k  ;  j++, k--  ) {
      c = s[j];
      s[j] = s[k];
      s[k] = c;
    }
  }
  
  dec = i;
  if ( !dec ) {        /* check for numbers less than 1 */
    if (type != 'f') { /* assume 'e' or 'E' format */
      if (d != 0) {    /* d == d might be a safety check? */
        while( (d *= 10) < 1 && dec > DBL_MIN_EXP )
          --dec;
        --dec;
      }
      if (d >= 1)
        prec++, d /= 10;
    } else s[i++] = '0';
  }
  
  if (dec < 0) width = prec;
  else if (type == 'f') width = prec + (dec > 0 ? dec : 1);
  else width = prec + 1; M = DBL_EPSILON/(FLT_RADIX*2.0); /* precision value */

  while ( 1 ) {        /* now collect fraction */
    d *= 10;
    j = (int) d;
    d -= j;
    M *=10;
    if( (i < width) && (d >= M && d <= (1-M)) ) s[i++] = (char) j + '0';
    else break;
  }
  
  if(d>=0.5) j++;
  s[i++] = (char) j + '0';

  while(i<=width)
    s[i++] = '0'; /**Have to round see ANSI176,13.9.5.2 **/
//  if (!strround(s, width + 1)) /* watch for over flow */
//    s[0] = '1', width++, dec++;
  if (p != 0) {                /* add in decimal point */
    if (type == 'f') { 
      if (dec > 0) spt = dec;
      else spt = 1;
    } else spt = 1;
    for (i = width; i > spt; i--)
      s[i] = s[i - 1]; s[spt] = '.';
  } else width--;
  
  if (trunc) { /* remove trailing zeros */
    while (width && s[width] == '0') width--;
    if (s[width] == '.') width--;
  }
  
  if (type != 'f') { /* add in exponent */
    s[++width] = type;
    if (dec >= 0) {
      s[++width] = '+';
      if (dec != 0) dec = dec - 1;
      if (dec < 10) s[++width] = '0';
    } else if (dec < 0) {
      s[++width] = '-';
      if (dec > -10) s[++width] = '0';
      dec = -dec;
    }
    df_itoa(dec, &s[++width], 10);
  } else s[width + 1] = 0;
  
  return str;
}

void Solve ( void ) {
  FormPtr formP = FrmGetActiveForm();
  FlpCompDouble a, b, c, d, f, radical, x1, x1a, x2, x2a;
  Char s[128], t[128], *p;
  Boolean negative = false;
  
  FlpBufferAToF( &a.fd, FldGetTextPtr ( GetObjectPtr ( formP, MainAValue ) ) );
  FlpBufferAToF( &b.fd, FldGetTextPtr ( GetObjectPtr ( formP, MainBValue ) ) );
  FlpBufferAToF( &c.fd, FldGetTextPtr ( GetObjectPtr ( formP, MainCValue ) ) );

  if ( a.d == 0 ) a.d = 1.0;
  
  radical.d = (b.d * b.d) - 4*a.d*c.d;
  SetFieldTextFromStr ( formP, MainDiscriminant, fftoa ( radical.d, s, 10, 'f', 1 ) );
  
  if ( radical.d < 0 ) negative = true;
  
  if ( negative ) {
    FrmMyAlert ("Negative");
  
    radical.d *= -1;
    radical.d = sqrt(radical.d);

    x1.d = ( b.d * -1 ) / ( 2 * a.d );
    x1a.d = radical.d / ( 2 * a.d );
    StrCopy ( s, fftoa ( x2.d, s, 5, 'f', 1 ) );
    StrCopy ( t, fftoa ( x2a.d, t, 5, 'f', 1 ) );
    StrCat ( s, " + " );
    StrCat ( s, t );
    StrCat ( s, " i");

    SetFieldTextFromStr ( formP, MainXValue, s );
    
    x2.d = ( b.d * -1 ) / ( 2 * a.d );
    x2a.d = radical.d / ( 2 * a.d );
    StrCopy ( s, fftoa ( x2.d, s, 5, 'f', 1 ) );
    StrCopy ( t, fftoa ( x2a.d, t, 5, 'f', 1 ) );
    FlpFToA ( x2.fd, s );
    FlpFToA ( x2a.fd, t );
    StrCat ( s, " + " );
    StrCat ( s, t );
    StrCat ( s, " i");
    
    SetFieldTextFromStr ( formP, MainYValue, s );
    
  } else {

    radical.d = sqrt(radical.d);
    x1.d = ( ( b.d * -1 ) + radical.d ) / ( 2 * a.d );
    x2.d = ( ( b.d * -1 ) - radical.d ) / ( 2 * a.d );
    
    SetFieldTextFromStr ( formP, MainXValue, fftoa ( x1.d, s, 10, 'f', 1 ) );
    SetFieldTextFromStr ( formP, MainYValue, fftoa ( x2.d, s, 10, 'f', 1 ) );
    
    StrCopy ( s, "(x" );
    if ( x1.d < 0 ) {
      StrCat ( s, " + " );
      StrCat ( s, fftoa ( -(x1.d), t, 5, 'f', 1 ) );
    } else if ( x1.d != 0 ) {
      StrCat ( s, " - " );
      StrCat ( s, fftoa ( (x1.d), t, 5, 'f', 1 ) );
    }
    
    StrCat ( s, ")(x" );

    if ( x2.d < 0 ) {
      StrCat ( s, " + " );
      StrCat ( s, fftoa ( -(x2.d), t, 5, 'f', 1 ) );
    } else if ( x2.d != 0 ) {
      StrCat ( s, " - " );
      StrCat ( s, fftoa ( (x2.d), t, 5, 'f', 1 ) );
    }

    StrCat ( s, ")" );
    
    SetFieldTextFromStr ( formP, MainFactor, s );

  }
  
  if ( a.d > 0 ) StrCopy ( s, "up." );
  else StrCopy ( s, "up." );
  SetFieldTextFromStr ( formP, MainOpens, s );
  
  radical.d = (b.d * b.d) - 4*a.d*c.d;

  x1.d = -b.d / 2 / a.d;
  x2.d = -radical.d / 4 / a.d;
  
  StrCopy ( s, "(" );
  StrCat ( s, fftoa ( x1.d, t, 5, 'f', 1 ) );
  StrCat ( s, ", " );
  StrCat ( s, fftoa ( x2.d, t, 5, 'f', 1 ) );
  StrCat ( s, ")" );
  
  SetFieldTextFromStr ( formP, MainVertex, s );

}

Boolean Main_Form_Handle_Event(EventPtr event) {
  Boolean      handled = false;
  FormPtr      formP   = NULL,
               popP    = NULL;
  
  formP = FrmGetActiveForm();

  switch(event->eType) {
    case frmOpenEvent:
      FrmDrawForm ( formP );
      handled = true;
      break;

    case frmCloseEvent:
      handled = false;
      break;

    case ctlSelectEvent:
      switch ( event->data.ctlSelect.controlID ) {
        case MainSolveButton:
          Solve();
          break;

        default:
          break;
      }
      break;

    case menuEvent:
      MenuEraseStatus ( 0 );
      switch ( event->data.menu.itemID ) {

        case HelpAbout:
          MenuEraseStatus( 0 );       // Clear the menu status from the display.

          popP = FrmInitForm ( AboutForm );
          FrmDoDialog ( popP );       // Display the About Box.
          FrmDeleteForm ( popP );

          handled = true;
          break;
      }
          
    default:
      handled = false;
      break;
  }

  return ( handled );
}
