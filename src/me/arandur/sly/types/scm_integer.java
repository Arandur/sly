/**
 * Class representing a Scheme integer
 */

package me.arandur.sly.types;

import java.math.BigInteger;
import java.util.Arrays;

public class Scheme_Integer implements Scheme_Object
{
  private BigInteger val;

  /*
   * Constructors
   */
  public Scheme_Integer (BigInteger _val)
  {
    val = _val;
  }

  public Scheme_Integer (Integer _val)
  {
    this (_val.longValue ());
  }

  public Scheme_Integer (long _val)
  {
    val = BigInteger.valueOf (_val);
  }

  public Scheme_Integer (String _val)
  {
    this (BigInteger (_val));
  }

  /*
   * Scheme_Object interface
   */
  public Iterable<String> repr ()
  {
    return Arrays.asList (val.toString ());
  }

  /*
   * Getter
   */
  public BigInteger.getValue ()
  {
    return val;
  }
}
