#include <Foundation.h/Foundation>

int main (int argx, const char * argv[])
{

  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

  NSLog (@"hello world");


  [pool drain];
  return 0;
