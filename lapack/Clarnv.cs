using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLARNV returns a vector of n random complex numbers from a uniform or
        /// normal distribution.
        /// </para>
        /// </summary>
        /// <param name="idist">
        /// [in] IDIST is INTEGER.
        /// Specifies the distribution of the random numbers:
        /// = 1:  real and imaginary parts each uniform (0,1)
        /// = 2:  real and imaginary parts each uniform (-1,1)
        /// = 3:  real and imaginary parts each normal (0,1)
        /// = 4:  uniformly distributed on the disc abs(z) &lt; 1
        /// = 5:  uniformly distributed on the circle abs(z) = 1
        /// </param>
        /// <param name="iseed">
        /// [in,out] ISEED is INTEGER array, dimension (4).
        /// On entry, the seed of the random number generator; the array
        /// elements must be between 0 and 4095, and ISEED(4) must be
        /// odd.
        /// On exit, the seed is updated.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of random numbers to be generated.
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX array, dimension (N).
        /// The generated random numbers.
        /// </param>
        /// <remarks>
        /// <para>
        ///  This routine calls the auxiliary routine SLARUV to generate random
        ///  real numbers from a uniform (0,1) distribution, in batches of up to
        ///  128 using vectorisable code. The Box-Muller method is used to
        ///  transform numbers from a uniform to a normal distribution.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clarnv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clarnv(
            int idist,
            int* iseed,
            int n,
            Complex32* x);
    }
}
