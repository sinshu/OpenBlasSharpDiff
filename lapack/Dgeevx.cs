using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEEVX computes for an N-by-N real nonsymmetric matrix A, the
        /// eigenvalues and, optionally, the left and/or right eigenvectors.
        /// </para>
        /// <para>
        /// Optionally also, it computes a balancing transformation to improve
        /// the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
        /// SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
        /// (RCONDE), and reciprocal condition numbers for the right
        /// eigenvectors (RCONDV).
        /// </para>
        /// <para>
        /// The right eigenvector v(j) of A satisfies
        ///                  A * v(j) = lambda(j) * v(j)
        /// where lambda(j) is its eigenvalue.
        /// The left eigenvector u(j) of A satisfies
        ///               u(j)**H * A = lambda(j) * u(j)**H
        /// where u(j)**H denotes the conjugate-transpose of u(j).
        /// </para>
        /// <para>
        /// The computed eigenvectors are normalized to have Euclidean norm
        /// equal to 1 and largest component real.
        /// </para>
        /// <para>
        /// Balancing a matrix means permuting the rows and columns to make it
        /// more nearly upper triangular, and applying a diagonal similarity
        /// transformation D * A * D**(-1), where D is a diagonal matrix, to
        /// make its rows and columns closer in norm and the condition numbers
        /// of its eigenvalues and eigenvectors smaller.  The computed
        /// reciprocal condition numbers correspond to the balanced matrix.
        /// Permuting rows and columns will not change the condition numbers
        /// (in exact arithmetic) but diagonal scaling will.  For further
        /// explanation of balancing, see section 4.10.2 of the LAPACK
        /// Users&#39; Guide.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="balanc">
        /// [in] BALANC is CHARACTER*1.
        /// Indicates how the input matrix should be diagonally scaled
        /// and/or permuted to improve the conditioning of its
        /// eigenvalues.
        /// = &#39;N&#39;: Do not diagonally scale or permute;
        /// = &#39;P&#39;: Perform permutations to make the matrix more nearly
        /// upper triangular. Do not diagonally scale;
        /// = &#39;S&#39;: Diagonally scale the matrix, i.e. replace A by
        /// D*A*D**(-1), where D is a diagonal matrix chosen
        /// to make the rows and columns of A more equal in
        /// norm. Do not permute;
        /// = &#39;B&#39;: Both diagonally scale and permute A.
        /// 
        /// Computed reciprocal condition numbers will be for the matrix
        /// after balancing and/or permuting. Permuting does not change
        /// condition numbers (in exact arithmetic), but balancing does.
        /// </param>
        /// <param name="jobvl">
        /// [in] JOBVL is CHARACTER*1.
        /// = &#39;N&#39;: left eigenvectors of A are not computed;
        /// = &#39;V&#39;: left eigenvectors of A are computed.
        /// If SENSE = &#39;E&#39; or &#39;B&#39;, JOBVL must = &#39;V&#39;.
        /// </param>
        /// <param name="jobvr">
        /// [in] JOBVR is CHARACTER*1.
        /// = &#39;N&#39;: right eigenvectors of A are not computed;
        /// = &#39;V&#39;: right eigenvectors of A are computed.
        /// If SENSE = &#39;E&#39; or &#39;B&#39;, JOBVR must = &#39;V&#39;.
        /// </param>
        /// <param name="sense">
        /// [in] SENSE is CHARACTER*1.
        /// Determines which reciprocal condition numbers are computed.
        /// = &#39;N&#39;: None are computed;
        /// = &#39;E&#39;: Computed for eigenvalues only;
        /// = &#39;V&#39;: Computed for right eigenvectors only;
        /// = &#39;B&#39;: Computed for eigenvalues and right eigenvectors.
        /// 
        /// If SENSE = &#39;E&#39; or &#39;B&#39;, both left and right eigenvectors
        /// must also be computed (JOBVL = &#39;V&#39; and JOBVR = &#39;V&#39;).
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the N-by-N matrix A.
        /// On exit, A has been overwritten.  If JOBVL = &#39;V&#39; or
        /// JOBVR = &#39;V&#39;, A contains the real Schur form of the balanced
        /// version of the input matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="wr">
        /// [out] WR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="wi">
        /// [out] WI is DOUBLE PRECISION array, dimension (N).
        /// WR and WI contain the real and imaginary parts,
        /// respectively, of the computed eigenvalues.  Complex
        /// conjugate pairs of eigenvalues will appear consecutively
        /// with the eigenvalue having the positive imaginary part
        /// first.
        /// </param>
        /// <param name="vl">
        /// [out] VL is DOUBLE PRECISION array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left eigenvectors u(j) are stored one
        /// after another in the columns of VL, in the same order
        /// as their eigenvalues.
        /// If JOBVL = &#39;N&#39;, VL is not referenced.
        /// If the j-th eigenvalue is real, then u(j) = VL(:,j),
        /// the j-th column of VL.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
        /// u(j+1) = VL(:,j) - i*VL(:,j+1).
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.  LDVL &gt;= 1; if
        /// JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is DOUBLE PRECISION array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right eigenvectors v(j) are stored one
        /// after another in the columns of VR, in the same order
        /// as their eigenvalues.
        /// If JOBVR = &#39;N&#39;, VR is not referenced.
        /// If the j-th eigenvalue is real, then v(j) = VR(:,j),
        /// the j-th column of VR.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
        /// v(j+1) = VR(:,j) - i*VR(:,j+1).
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.  LDVR &gt;= 1, and if
        /// JOBVR = &#39;V&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="ilo">
        /// [out] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [out] IHI is INTEGER.
        /// ILO and IHI are integer values determined when A was
        /// balanced.  The balanced A(i,j) = 0 if I &gt; J and
        /// J = 1,...,ILO-1 or I = IHI+1,...,N.
        /// </param>
        /// <param name="scale">
        /// [out] SCALE is DOUBLE PRECISION array, dimension (N).
        /// Details of the permutations and scaling factors applied
        /// when balancing A.  If P(j) is the index of the row and column
        /// interchanged with row and column j, and D(j) is the scaling
        /// factor applied to row and column j, then
        /// SCALE(J) = P(J),    for J = 1,...,ILO-1
        /// = D(J),    for J = ILO,...,IHI
        /// = P(J)     for J = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <param name="abnrm">
        /// [out] ABNRM is DOUBLE PRECISION.
        /// The one-norm of the balanced matrix (the maximum
        /// of the sum of absolute values of elements of any column).
        /// </param>
        /// <param name="rconde">
        /// [out] RCONDE is DOUBLE PRECISION array, dimension (N).
        /// RCONDE(j) is the reciprocal condition number of the j-th
        /// eigenvalue.
        /// </param>
        /// <param name="rcondv">
        /// [out] RCONDV is DOUBLE PRECISION array, dimension (N).
        /// RCONDV(j) is the reciprocal condition number of the j-th
        /// right eigenvector.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, the QR algorithm failed to compute all the
        /// eigenvalues, and no eigenvectors or condition numbers
        /// have been computed; elements 1:ILO-1 and i+1:N of WR
        /// and WI contain eigenvalues which have converged.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgeevx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgeevx(
            MatrixLayout matrixLayout,
            char balanc,
            char jobvl,
            char jobvr,
            char sense,
            int n,
            double* a,
            int lda,
            double* wr,
            double* wi,
            double* vl,
            int ldvl,
            double* vr,
            int ldvr,
            int* ilo,
            int* ihi,
            double* scale,
            double* abnrm,
            double* rconde,
            double* rcondv);
    }
}
