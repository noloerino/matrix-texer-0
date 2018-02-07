def frac_tag(s1, s2):
    """
    Applys a LaTeX "\frac" tag to two arguments.
    """
    return apply_tag("frac", s1, s2)

def sqrt_tag(n):
    return apply_tag("sqrt", n)

def apply_tag(tag, *args):
    """
    Applys a LaTeX tag, specified by the tag argument, to the supplied
    arguments.
    """
    text = "\\" + tag
    for arg in map(str, args):
        text += "{" + arg + "}"
    return text

def row_to_latex(row):
    """
    Takes a row of a matrix and converts it to what it would be in LaTeX,
    by interspersing ampersands between elements and appending two backslashes
    for a newline.
    """
    if len(row) == 0:
        return "\\\\"
    line = str(row[0])
    for i in range(1, len(row)):
        line += " & " + str(row[i])
    return line + " \\\\"

def matrix_to_latex(mat, indent=4, math_tags=True):
    """
    Takes a matrix and converts it to LaTeX.
    The indent parameter sets the number of spaces used per indent level.
    The math_tags parameter indicates whether or not to include wrapping $$.
    """
    text = ("$$ " if math_tags else "") + "\\begin{bmatrix}\n"
    for row in mat:
        text += " " * indent + row_to_latex(row) + "\n"
    return text + "\\end{bmatrix}" + (" $$" if math_tags else "")

def show_mat(x):
    print(matrix_to_latex(x))

# edit this, then run the program
f = frac_tag
s = sqrt_tag
ln = lambda s: "ln(" + str(s) + ")"

mat = [
    [2, 1, 0, 0, 17],
    [0, 2, 3, 0, 37],
    [1, 1, 1, 0, 20],
    [1, 1, 0, 1, 20]
]

#################################
show_mat(mat)