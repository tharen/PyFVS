//----------
//  $Id: stdafx.h 1276 2014-07-07 14:59:41Z sarebain@fs.fed.us $
//----------
// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#ifndef _WIN32_WINNT		// Allow use of features specific to Windows XP or later.                   
#define _WIN32_WINNT 0x0501	// Change this to the appropriate value to target other versions of Windows.
#endif						

#include <stdio.h>
#ifndef unix
#include <tchar.h>
#endif

// TODO: reference additional headers your program requires here
