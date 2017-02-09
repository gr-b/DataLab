/* 
 * CS:APP Data Lab 
 * 
 * Griffin Bishop
 * grbishop 
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
 4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* 
 * bitNor - ~(x|y) using only ~ and & 
 *   Example: bitNor(0x6, 0x5) = 0xFFFFFFF8
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitNor(int x, int y) {
  // This one was explained in lab
  return (~x) & (~y);
} 


/* 
 * copyLSB - set all bits of result to least significant bit of x
 *   Example: copyLSB(5) = 0xFFFFFFFF, copyLSB(6) = 0x00000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int copyLSB(int x) {
  /* Shift the bit over 31 times
   This will give us a number with either 32 zeroes or
   32 ones.
   You don't have to isolate the LSB first because
   if you shift the number 31 times, the only bit left will 
   be the LSB of the original number.
   This one was explained in lab  */
  return (x << 31 ) >> 31;

}

/* 
 * reverseBytes - reverse the bytes of x
 *   Example: reverseBytes(0x01020304) = 0x04030201
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
int reverseBytes(int x) {
  /* Make 8 bit masks and manually reverse the bytes.
   There's 32 bits and each byte has 8 bits so there's 4 bytes to reverse
   the first mask is all zeros except for the 8 least significant bits
   1111 1111 is  byte1 = (((byte1 << 8) << 8) << 8); */ 
  int mask1 = 255; // mask1 now fills the first byte with 1s  
  int mask2 = mask1 << 8;
  int mask3 = mask2 << 8;
  int mask4 = mask3 << 8;
  int byte1 = x & mask1;
  int byte2, byte3, byte4, newx;
  
  byte1 = byte1 << 24;
  
  byte2 = x & mask2;
  byte2 = (byte2 << 8);
  
  byte3 = x & mask3;
  byte3 = (byte3 >> 8) & mask2; 
  
  byte4 = x & mask4;
  byte4 = (((byte4 >> 8) >> 8) >> 8) & mask1;
  // Put them in the right place, then or them together

  newx = byte1 | byte2 | byte3 | byte4;
  return newx;
}


/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {
  int temp, mask, lsb;  
  /* ~x + 1 gives the negative of x
   So the sign bit will be as follows:
   For a Positive nonzero x: 
   sign bit is always 0 to start
   will always be 1, for example:
   x = 0010 (using 4 bits instead of 32, so MSB(1st on left) is sign bit
   ~x = 1101
   ~x+1 = 1110 
   For a negative nonzero x:
   Sign bit is always 1 to start
   Will always be 0, except for Tmin, e. g:
   x = 1001 (non tmin)
   ~x = 0110
   ~x+1 = 0111
   For x = tmin:
   x = 1000
   ~x = 0111
   ~x+1 = 1000
   So MSB is 1
   For zero:
   x = 0000
   ~x = 1111
   ~x + 1 = 0000
   So this operation will flip the original sign bit
   for for nonzero numbers,
   and for zero it will keep it the same.
   So we can just or the expression with the original to
   get it so the MSB is always 1 for nonzeros
   and 0 for zero.*/
  
  temp = (~x + 1) | x;
  
  //Now we need to isolate the MSB
  //Use a bit mask 
  mask = 1 << 31; // a 1 at MSB
  temp  = temp & mask; 
  
  // But the ! operator should give 1 if x is 0, and 0 if x is not,
  // So we need to reverse it
  temp = ~temp;
  
  // But now we'll have 1000 if x is zero, and 0111 if not
  // So we can just do a right shift to fill in the rest
  temp = temp >> 31;
  
  // Now it works for non-zero, but will be all 1's if x is zero,
  // so get the LSB
  lsb = temp & 1;
  return lsb;
}
/* 
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
  int msbMask;
  /* For non-zero values: just isolate the sign bit
   for zero values:
   Example: 
         Pos:    Neg:   Zero:
   x     0010    1001   0000 
   ~x    1101    0110   1111
   The sign bit for positives and zeros
   is the same. isolate the MSB*/
  x = ~x;
  msbMask = 1 << 31;
  x = x & msbMask;
  
  // But we need to return zero or one, not zero or tmin,
  // so get just that bit
  x = (x >> 31) & 1; 
  return x;
}
/* 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
  int msbMask, xMsb, yMsb, sumMsb, different, same, overflow, okay;  
  /* We're only concerned with when two positives add to be too big,
   or when two negatives add to be too small.
   because if a large positive and a larger negative are added,
   the result will be negative as expected, and the other way 
   around.*/

  // So we only care about the sign bits of the numbers given.
  msbMask = 1 << 31; // Make a mask for the MSB
  xMsb = x & msbMask;
  yMsb = y & msbMask;
  sumMsb = (x+y) & msbMask;

  // But we want them to be either all zero or all one based on their sign
  xMsb = xMsb >> 31;
  yMsb = yMsb >> 31;
  sumMsb = sumMsb >> 31;

  // So, we only care if the sign is the same
  // XOR tells us if they are different, so we can use that:
  different = (xMsb^yMsb);
  same = ~different; 
  // Same is 1 if they are the same.
  
  // If they are the same, we can check the sign
  // of the sum, because it won't be the same as
  // the two we are talking about if it overflowed
  // e. g.: If two negatives sum to a positive, 
  // something is wrong. If two positives sum to 
  // a negative, something is wrong
  // Again, ^ will return 1 if they are the different
  // So this says that if x and y have the same sign,
  // but x has a different sign than the sum,
  // return 1(not okay). If not, return 0 (okay)
  overflow = same & ( xMsb^sumMsb );
  
  // Problem specifies that it should return 0 if it's not
  // okay, and 1 if it is okay. 
  okay = !overflow; 
   
  return okay;  
}
/* 
 * rempwr2 - Compute x%(2^n), for 0 <= n <= 30
 *   Negative arguments should yield negative remainders
 *   Examples: rempwr2(15,2) = 3, rempwr2(-35,3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int rempwr2(int x, int n) {
    int shifted, mask, remainder, sign, negative;    
    // Shifting to the right is like dividing by 2^n where
    // n is the number you shift by.
    // so x<<n is x/2^n
    // We're only worried about the remainder
    // which in this case, we don't even have to shift to get
    // The remainder of dividing by 2^n (which is the same as
    // x % (2^n) is the same as taking the value of the n bits
    // that would have been shifted out.
    // We can just make a bit mask for the n least significant bits
    
    shifted = (1 << n);
    mask = shifted + ~0;
    
    // After the shifted bit is the remainder that we want, so subtract one
    // from the shifted bit number.

    // Use the mask:
    remainder = x & mask;

    // But what about the negatives!?  
    // (8 least sig.)  -37 : 1101 1011 (-32 + -4 + -1)
    // For negatives ~(-37): 0010 0100 
    //        with the mask: 0000 0111
    //              becomes: 0000 0100 which is 4, or one less
    //              than what we want. (also diff sign, but we can
    //              fix that easy)  
 
     
    // So get the sign of the number:
    sign = (x >> 31);
    

    // If positive, return the remainder. If negative,
    // invert and add one, then put in the sign.
    
    // Sign is 0 when positive, so 
    // when positive, return remainder
    // When negative, invert and add one,
    // and add in the sign.    
    negative = ~remainder + 1;
    
    // But we only want the last n bits of this.
    negative = negative & mask; 
    negative = ~negative + 1;
    
    // But this is positive. Add the sign bit in.
    // But only if current remainder is not zero.
    return ( ~sign  & remainder ) | ( sign & negative);
}
/* 
 * one_bit - return 1 if the argument has exactly one 1-bit in its
 *   binary representation. Otherwise, return zero.
 *   Example: one_bit(0x00800000) = 1; one_bit(10) = 0
 *   Legal ops: ! ~ & ^ | + - << >>
 *   Max ops: 10
 *   Rating: 3 
 */
int one_bit(int x) {
  int temp;
  // Could possible be done with 32 bit masks, but that would get dirty
  // Examples(4 bits instead of 32):
  // x          1111  0111  0001  0100  0010  1000
  // ~x         0000  1000  1110  1011  1101  0111
  // ~x+1       0001  1001  1111  1100  1110  1000
  // From this chart, for the numbers with one 1,
  // we can see that in the negative of x, (~x+1),
  // the bits are always 1 to the left of and including the spot
  // where the orginal 1 bit was.
  
  // x^(~x+1)   1110  1110  1110  1011  1101  0000
  // x|(~x+1)   1111  1111  1111  1100  1110  1000
  // x&(~x+!)   0001  0001  0001  0100  0010  1000
  // For x & (~x+1), it is always the same at the original if it only had
  // one bit in the original. If it had more than one bit in the original,
  // it's not the same.
  // for 0000 -> 0 & (~0+1) = 0 So that works.
  
  temp = x & ( ~x + 1 );
  
  // If the original had only one bit, temp should equal the original,
  // if the original had more than one bit, or zero, temp should not equal 
  // the original.
  // but we cannot use the == operator, so we can use 
  // (the answer to the quiz question) !(x^y)
  // but zero is still a problem then.
  // make a special case
  
  return !(x^temp) & !!x;
}
/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *  Examples: satMul3(0x10000000) = 0x30000000
 *            satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *            satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 3
 */
int satMul3(int x) {
    int x2, x3, signx, signx2, signx3, overflow, tmin, tmax; 
    // We looked at this one in lab
    // We're multiplying by three, but 
    // We can't use the arithmetic multiplation operator,
    
    x2 = x+x; // x2 is 2*x
    x3 = x2+x; // x3 is 3*x. We don't just do x3 = x+x+x because
    
    // We want to catch overflow if it happens in x+x as well
    // Now check the signs on each
    signx = x >> 31;
    signx2 = x2 >> 31;
    signx3 = x3 >> 31;
    // These three signs should all be the same if there was
    
    // no overflow.
    // We can use ^ to check if they are not all the same:
    // just check one against the other two
    overflow = (signx ^ signx2) | (signx ^ signx3);
    
    // Now overflow will hold a 1 if it overflowed,
    // or a 0 if not.
 
    // If it didn't overflow, return the multiplied value: x3
    // If it overflowed and original was negative, return tmin
    tmin = 1 << 31;
    
    // If it overflowed and the original was positive, return tmax
    tmax = ~tmin;

    // Now we just need to package this into a statement that will 
    // return the right thing
    // overflow is either all 1s or all zeros so we can just &
    // it with what we want to return in each case.

    return ( ~overflow & x3 ) | // Didn't overflow
           (overflow & signx & tmin) | // Negative overflow
	   (overflow & ~signx & tmax); // Positive overflow
}

/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
  int sign, dividedBy4, threeFourths, absX; 
  // We need to multiply x by 3/4ths
  // but we can't have it overflow.
  // To do this, we can divide by 4 first, 
  // and then multiply by three afterward.
  // since 3/4ths is not above one, the number can't
  // overflow this way. 
 

  // If we take the absolute value from down below,
  // Then we don't have to worry about positive vs negative.
  sign = x>>31;
  absX = ( (x>>31) & (~x+1) ) | ( ~(x>>31) & x );
  x = absX;
  
  // For any positive x,
  // we can just divide by 4, then 
  dividedBy4 = (~x+1) >> 2;  
  threeFourths = x + dividedBy4; // x -1/4x
    
  //threeFourths = dividedBy4+dividedBy4+dividedBy4;
  // If sign is positive, abs value didn't do anything,
  // So we don't have to change it back
  // If the sign is negative, abs value did change it, 
  // and so we have to change it back.
  return (~sign & threeFourths) | (sign & (~threeFourths +1));
}

/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  /* Add x and negative y. 
   * If x and y have the same sign:
   *  return the sign of that sum
   * (if x=+ and y=+, x<=y if signSum
   * is -) 
   * If x and y have a different sign:
   *  return the sign of 1
   * (if x=+ and y=-, x<=y should give 0
   *  and xsign is 0)
   */
  int sum, ifTheyAreEqual, xsign, ysign, sumSign, ifSignsAreSame, ifSignsAreDifferent,  
  ifSignsAreDifferentThenReturnX, ifSignsAreSameAndXAndYAreEqual, ifSignsAreSameAndXAndYAreNotEqual;
  
  // Get the signs  
  xsign = x >> 31;
  ysign = y >> 31;

  // Create some named situations  
  ifSignsAreDifferent = xsign^ysign;
  ifSignsAreDifferentThenReturnX = (ifSignsAreDifferent & xsign);
  ifSignsAreSame = ~ifSignsAreDifferent;
  ifTheyAreEqual = ((!(x^y)) << 31) >>31; // Get a full 32 bits of the result

  ifSignsAreSameAndXAndYAreNotEqual = (ifSignsAreSame & ~ifTheyAreEqual);
  ifSignsAreSameAndXAndYAreEqual = (ifSignsAreSame & ifTheyAreEqual);
  
  // Compute the negative sum.
  sum = (~x+1) + y;
  sumSign = sum >> 31;

  return !!(  ( ifSignsAreDifferentThenReturnX ) | ( ifSignsAreSameAndXAndYAreEqual ) | 
              ( ifSignsAreSameAndXAndYAreNotEqual & ~sumSign  )  );
}

/* 
 * absVal - absolute value of x
 *   Example: absVal(-1) = 1.
 *   You may assume -TMax <= x <= TMax
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int absVal(int x) {
  /* Get sign bit. If positive, return x.
   * If negative, return negative x = (~x+1)
   */
  int sign, negx, ifPosReturnNegx, ifNegReturnPosx;
  
  sign = x >> 31;
  negx = ~x+1;
  
  ifPosReturnNegx = (sign & negx);
  ifNegReturnPosx = (~sign & x);
  return ifPosReturnNegx | ifNegReturnPosx;
}
/* 
 * float_abs - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_abs(unsigned uf) {
  /* float:
   * Just & the float with a bit mask to get the sign bit.
   */
  unsigned mask, sign, expBits, flipped, expMask, mantissaBits, mantissaMask, areExpBitsAllOn;
  
  mask  = 0x80000000u; // Bit mask for the MSB (in this case, the sign bit) 
  sign = mask & uf; // Get the sign bit
  
  // But what if the number is NaN?
  // NaN occurs when the exponent is all 1s, but the mantissa is not all zeros.
  // Infinity occurs when the exponent is all 1s, but the mantiss is all zero.
  // exp mask: 0111 1111 1000 0000 ... 0000 = 0x7f800000
  expMask = 0x7f800000u;

  expBits = expMask & uf;
  
  // Check if the expBits are 1.
  areExpBitsAllOn = !(expBits^expMask); // !(x^y) is x==y
  
  // But to check if it's NaN, we also need to know that the mantissa bits are not all zero
  // mantissaMask = | 0 | 000 0000 0 | 111 1111 1111 ... 1111 |
  mantissaMask = 0x007fffffu;
  
  mantissaBits = mantissaMask & uf;

 
  // If all the exponent bits are on, and there is atleast one mantissa bit on, return.
  if(areExpBitsAllOn && mantissaBits){ 
    return uf;
  }
    
  if(sign){ //If the sign bit is 1, 
	  // toggle it.
    flipped = mask ^ uf;
    return flipped;
  } else { // If the sign bit is 0, 
          // Don't toggle it.    
    return uf;
  }

}
/* 
 * float_f2i - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int float_f2i(unsigned uf) {
  /* Get the sign, exponent, and mantissa bits separately.
   * If the exponent bits are all 1, return tmin
   * If exp is not zero, mantissa has implied 1 at front,
   *   so and that bit in: 0x00800000 or 0x800000.
   * If exp is zero, don't worry about adding anything
   * 
   * Now take the bias out of exp: subtract 127
   *   An integer only holds 32 bits, so if exp 32 or greater,
   *   return tmin.
   */
  unsigned number, thereIsOverflow;
  int sign, exp, mant, signMask, expMask, mantMask, toShift;
  signMask = 0x80000000u; // 1000 0000 0000 0000...
  expMask = 0x7f800000u;  // 0111 1111 1000 0000...
  mantMask = 0x007fffffu; // 0000 0000 0111 1111...

  sign = signMask & uf;
  exp = expMask & uf;
  mant = mantMask & uf;
  
  // If exp is not zero,
  if(exp){
    // we must add the implied bit to the mantissa 
    mant = mant | (1<<23); // Put a one at the 24th bit of mant. 
  }
 
  // Take the bias out of the exp.
  exp = exp >> 23;
  // If exp is less than 127 before subtraction,
  // return 0 as the answer will be less than one. 
  if( exp < 127 ){
    return 0;
  }    
  exp  = exp +(~127)+1; // subtract 127
 
  // The problem is that when exp is zero at this point,  
  // The bit we want to be at one is at the 24th bit.
  // So we need to shift to right by 24-e
  if(exp < 24){
    toShift = 24-exp-1;	  
    // Perform the shift:
    number = mant;
    number = number >> toShift;
    
    // Factor in the sign
    if(sign) { // If original was negative, make the answer negative 
      number = (~number + 1);
    }
    
    return number;
  }
  
  // Shift the mantissa left by exp
  // which is the same as multiplying it by 2^exp
  number = mant;
  toShift = exp; // toShift is signed
  while(toShift+1>1) { // While toShift > 0,
    //check if there will be overflow:
    // There will be overflow if the msb is on
    thereIsOverflow = number & signMask;
    if(thereIsOverflow){
      return 0x80000000u;
    }
    
    // If there won't be any overflow this time, 
    number = number << 1; // Multiply by 2
    toShift = toShift + ~0; // decrement toShift
  }
  return number;
}
/* 
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_half(unsigned uf) {
  /* Just subtract one from the exp */
  unsigned sign, exp, mant, signMask, expMask, mantMask, impliedBit, shiftedOut;
  
  signMask = 0x80000000u; // 1000 0000 0000 0000...
  expMask = 0x7f800000u;  // 0111 1111 1000 0000...
  mantMask = 0x007fffffu; // 0000 0000 0111 1111...

  sign = signMask & uf;
  exp = expMask & uf;
  mant = mantMask & uf;
  
  // We want to subtract one from the exponent.
  // However:
  //      - If exp is all zero and mantissa is not all zero:
  //            shift the mantissa instead of subtracting from exp.
  //            We're shifting to the right(division) so we don't 
  //            need to worry about overflow! (but what about rounding?!)      
  //            (This is denormalized)
  //     - If the exp is all 1s and the mantissa is all 0s, the number is
  //        (+/-) infinity:
  //             So return the argument.
  //     - If the exp is all 1s and the mantissa is not all 1s, the number is NaN.
  //             -> If the exp is all 1s, it's not a regular number.
  //             -> The regular numbers only happen from exp: 00000001 from 11111110
  //        (When exp is non-zero but non-max)
  //             So return the argument.
  //
  //      - If the exp and the mantissa are both all zeros, the number is zero.
  //        So the number divided in half should also be zero. Shouldn't have to
  //        worry about this case.
  //
  //      - For numbers in the range of the exp (exp non-zero and non-max),
  //        the mantissa is normalized and we can just decrement the exp by 1 to 
  //        divide by two.
  //      HOWEVER, if the exp is 1 (also -126), after we decrement the exponent, we
  //        have to add the implied 24th bit onto the mantissa before shifting.
  //
  //        So if exp is zero, don't decrement the exponent, but shift the mantissa to the right.   
  //////////////////////
  //  For floats:
  //  A float is normalized if it can only be represented one way.
  //  A float is denormalized if it can be represented in more than one way.
  //      - A float is normalized if its mantissa is between 0 and 1
  //      -> This only happens if the exponent is all zeros.
  //
  //                       Exp:                     Mantissa
  // Binary:           11111111               0000000000000000000000000     (Infinity)
  //                   (2^128)
  //                   11111111               1111111111111111111111111     (NaN)
  //                   11111111               1000010000100100101010101     (NaN)
  //                   (all 1s)                    (Not zero)               
  //                   
  //                   (is however, still normalized)
  //
  //                 -> If the exponent is all ones, the float represents either infinity or NaN.
  //
  //        Mantissa:  The mantissa is the decimal part, so it doesn't make sense to look at it from
  //                   left to right like a conventional binary number.
  //                   It is like the decimal was shifted 23 places over.
  //                   ->As you add bits from left to right, the number gets more precise
  //
  //                   The 24 bit mantissa can represent values from 0 to 1 by itself. 
  //                   This means it is denormalized. This only happens when the exponent
  //                   is all zero. 
  //                   When the exponent is not all zero, the float represents a normalized value
  //                   and with this, a 1 is implied to the left of the mantissa, shifting the range
  //                   of possible values to be from 1.0 to 2.0 (really 1.9999~)
  //
  //                   00000001               1000000000000000000000000     (M: 1.5)
  //             (nonzero, non-all-1s)     (1.1000*** implied)
  //                   00000001               1110000000000000000000000     (M: 1.85)
  //                                          (1.0 -> 2.0)
  //                   (Normalized)
  //
  ///////////////////////////////
  //                   (Denormalized:)
  //
  //                   00000000               0000000000000000000000000      ( zero )
  //                   (All-zero)             
  //                   00000000               1000000000000000000000000      (M: 0.5)
  //                                          (0.0 -> 1.0)
  //
  ///////////////////////////////
  //                   
  //                   00000001               0000000000000000000000000      (Smallest float)
  //            (=1. no bias: -126)           
  //                   11111110               1111111111111111111111111      (largest float)
  //            (=254. no bias: 127)
  //            decimal ranges from 1 to 254.  Without Bias ranges from -126 to 127
  //////////////////////////////

  // Shift exp over so we can work with it.
  exp = exp >> 23;

  // cases:
  if(exp==255){
    // If exp is all ones
    // The float is either infinity, or NaN,
    // so return it.
    return uf;
  } else if(exp==1){
    // If exp is 1,
    // Decrement exp to zero
    // and shift the mantissa to the right,
    // only after adding the implied bit onto the left of it
    
    // But first check special cases - mant being 0 or 1
    if(mant == 1){
      mant = 0;
    }
    impliedBit = mantMask+1; // The implied bit is right to the left
                             // of the mantissa bits.
    mant = mant + impliedBit;
    shiftedOut = mant & 1;
    mant = mant >> 1;
    if(shiftedOut && (mant & 1) ){
      // If there was a bit shifted out, and we need to round up
      mant = mant + 1;
    }
    // exp is 1, so decrement it to zero (set it)
    exp = 0; 
  } else if(exp==0){
    // If the exponent is zero, the mantissa is not implied before this
    // operation, and so we don't have to add in the implied bit.
    // Just shift the mantissa, and leave the exponent as zero
    
    // But first check the special cases:
    if(mant == 1){
      mant = 0;
    } 

    shiftedOut = mant & 1; // The bit that will get shifted out.    
    mant = mant >> 1;
    if(shiftedOut && (mant & 1)){
      // If a bit was shifted out, add one to the answer.
      // Unless the mantissa is zero or one, because in those cases, the 
      // should be zero.    
      mant = mant + 1;
    }
  } else {
    // The exponent should be more than 1 and less than 255 now.
    // If it isn't, then we have a problem,
    if( !(exp>1 && exp < 255) ){
      // So tell me that there was a problem.
      return 0xdeadfaceu; // Funny joke, amirite?
            // Valid hex
    }
    // If we don't have a problem,
    // and the exponent is more than 1 and
    // less than 255, then we can
    // decrement the exponent, 
    // and leave the mantissa alone
    exp = exp - 1;
  }

  // Now we just need to put it all back together.
  // Shift the exp back into place:
  exp = exp << 23;

  return sign | exp | mant; // Fit together.
}
