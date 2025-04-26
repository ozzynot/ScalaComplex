class Complex(val real: Double, val imaginary: Double = 0.0) {

  override def toString: String = {
    if (imaginary == 0) s"$real"
    else if (imaginary < 0) s"$real - ${-imaginary}i"
    else s"$real + ${imaginary}i"
  }

  def +(that: Complex): Complex =
    new Complex(this.real + that.real, this.imaginary + that.imaginary)

  def +(d: Double): Complex =
    new Complex(this.real + d, this.imaginary)

  def -(that: Complex): Complex =
    new Complex(this.real - that.real, this.imaginary - that.imaginary)

  def *(that: Complex): Complex = {
    val r = this.real * that.real - this.imaginary * that.imaginary
    val i = this.real * that.imaginary + this.imaginary * that.real
    new Complex(r, i)
  }

  def *(d: Double): Complex =
    new Complex(this.real * d, this.imaginary * d)

  def /(that: Complex): Complex = {
    val denom = that.real * that.real + that.imaginary * that.imaginary
    val r = (this.real * that.real + this.imaginary * that.imaginary) / denom
    val i = (this.imaginary * that.real - this.real * that.imaginary) / denom
    new Complex(r,i)
  }

  def conjugate: Complex =
    new Complex(real, -imaginary)

  def reciprocal: Complex = {
    val denom = real * real + imaginary * imaginary
    new Complex(real / denom, -imaginary / denom)
  }

}
