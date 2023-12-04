:- use_module(library(csv)).

% Predicate to read data from a CSV file and store it as rules
read_csv_and_store(Filename) :- csv_read_file(Filename, Rows, []),
process_rows(Rows).
% Process each row in the CSV file and store data as rules
process_rows([]).
process_rows([Row|Rows]) :- process_row(Row), process_rows(Rows).
% Store data from a row as a rule
%
process_row(row(EEID, FullName, JobTitle, Department, BusinessUnit, Gender, Ethnicity, Age, HireData, AnnualSalary, Bonus, Country, City, ExitDate)) :-
    assert(employee(EEID, FullName, JobTitle, Department, BusinessUnit, Gender, Ethnicity, Age, HireData, AnnualSalary, Bonus, Country, City, ExitDate)).
retract(employee('EEID', _, _, _, _, _, _, _, _, _, _, _, _, _)). 
% Rule to determine if an employee is working in Seattle

is_seattle_employee(Name) :- employee(_, Name, _, _, _, _, _, _, _, _, _, _, 'Seattle', _).


% Rule to determine if an employee is a senior manager in the IT department 
is_senior_manager_in_IT(Name) :- employee(_, Name, 'Sr. Manger', 'IT', _, _, _, _, _, _, _, _, _, _).




% Rule to etermine if an employee is a Director in the Finance department and works in Miami 
is_director_finance_miami(Name) :- employee(_, Name, 'Director', 'Finance', _, _, _, _, _, _, _, _, 'Miami', _).



% Rule to determine if an employee is from the United Stated, wroks in manufacturaing, older than 40, Asian and Male. 
is_asian_US_manufacturing_40M(Name, BusinessUnit, Gender, Ethnicity, Age) :- employee(_, Name, _, _, BusinessUnit, Gender, 'Asian', Age, _, _, _, 'United States', _, _).

% Rule to greet an employee
 greet(EEID) :- employee(EEID, FullName, JobTitle, Department, BusinessUnit, _, _, _, _, _, _, _, _, _),format("Hello, ~w, ~w of ~w, ~w!~n", [FullName, JobTitle, Department, BusinessUnit]).






% Rule to compute years until retirement. Assume retirement age is 65.
 years_until_retirement(Name, Age, Years_to_retire) :-  retirement_age(RetirementAge), Years_to_retire is RetirementAge - Age.

% Define retirement age
retirement_age(65).



% Rule to determine Research & Development employees with Black Ethnicity, within the age 25-50% 
 is_rd_black_midAge(Name, BusinessUnit, Ethnicity, Age) :- employee(_, Name, _, _, 'Research & Development', _, 'Black', Age, _, _, _, _, _, _), Age >= 25, Age =< 50.

% Rule to determine if an employee is from IT or Finance AND from Pheonic or Miami or Austin
is_ITorFin_PHXorMIAorAUS(Name, Department, City) :- employee(_, Name, _, Department, _, _, _, _, _, _, _, _, City, _), (Department = 'IT' ; Department = 'Finance'), (City = 'Phoenix' ; City = 'Miami' ; City = 'Austin').


% Rule to determine female employees in senior roles
 is_female_senior_role(Name, JobTitle) :- employee(_, Name, Title, _, _, 'Female', _, _, _, _, _, _, _, _), atom_concat('Sr.', _, Title). 


% Rule to determine if an employee is a highly paid senior manager
  is_highly_paid_senior_manager(Name, Salary) :- employee(_, Name, 'Sr. Manger', _, _, _, _, _, _, AnnualSalary, _, _, _, _), remove_dollar_comma(AnnualSalary, CleanedSalary), atom_number(CleanedSalary, SalaryNumeric), SalaryNumeric > 120000. % Helper predicate to remove dollar sign and commas from salary
 remove_dollar_comma(AnnualSalary, CleanedSalary) :- atom_chars(AnnualSalary, RawChars), include(is_digit, RawChars, Digits), atom_chars(CleanedSalary, Digits).
 
% Helper predicate to check if a character is a digit
  is_digit(Char) :- char_type(Char, digit).


% Rule to determine if an emplpyee's age is a prime number 
is_prime_age(Name, Age) :- employee(_, Name, _, _, _, _, _, Age, _, _, _, _, _, _), prime(Age).

prime(2).
prime(3).
prime(N) :- integer(N), N > 3, N mod 2 =\= 0, \+ has_factor(N, 3).
has_factor(N, Factor) :- N mod Factor =:= 0.
has_factor(N, Factor) :- Factor * Factor < N, NextFactor is Factor + 2, has_factor(N, NextFactor).


% Rule to determine average salary for a specified job title
average_salary(JobTitle, AvgSalary) :-
    findall(Salary, (
        employee(_, _, JobTitle, _, _, _, _, _, _, AnnualSalary, _, _, _, _),
        atom_chars(AnnualSalary, SalaryChars),
        exclude(is_separator, SalaryChars, CleanChars),
        atom_chars(CleanSalary, CleanChars),
        atom_number(CleanSalary, Salary)
    ), Salaries),
    length(Salaries, NumEmployees),
    sum_list(Salaries, TotalSalary),
    AvgSalary is TotalSalary / NumEmployees.

is_separator(',') :- !.
is_separator(' ').
is_separator('$').

% Rule to compute the total salary of person
% Total salary = Annual salary + (AnnualSalary*(Bonus/100))
% average_salary(JobTitle, Salary). 
total_salary(Name, Salary) :- employee(_, Name, _, _, _, _, _, _, _, AnnualSalary, Bonus, _, _, _), clean_and_convert(AnnualSalary, AnnualSalaryNum), clean_and_convert(Bonus, BonusNum), Salary is AnnualSalaryNum + (AnnualSalaryNum * (BonusNum / 100)).
clean_and_convert(Value, NumericValue) :-
    atom_string(Value, StrValue),
    string_chars(StrValue, Chars),
    include(type_digit, Chars, NumChars),
    string_chars(NumString, NumChars),
    atom_number(NumString, NumericValue).

type_digit(Char) :- char_type(Char, digit).

% Rule to determine take-home salary after tax
takehome_salary(Name, Job_Title, TakeHomeSalary) :- total_salary(Name, TotalSalary), determine_tax_percentage(TotalSalary, TaxPercentage), TakeHomeSalary is TotalSalary - (TotalSalary * (TaxPercentage / 100)). determine_tax_percentage(Salary, TaxPercentage) :- (Salary < 50000 -> TaxPercentage = 20 ; Salary >= 50001, Salary =< 100000 -> TaxPercentage = 25 ; Salary >= 100001, Salary =< 200000 -> TaxPercentage = 30 ; Salary > 200000 -> TaxPercentage = 35 ). 


% Rule (rules) to determine years of service. If the mployee has exited, Total Years = ExitDate-HireDate. If the employee has not exited, Total years = CurrentDate-HireDate. total_years(Name, Years). 
% se_module(library(date)).

total_years(Name, TotalYears) :-
    employee(_, Name, _, _, _, _, _, _, HireDate, _, _, _, _, ExitDate),

    current_date(CurrentDate),
    parse_date(HireDate, HireYear, HireMonth, HireDay),
    parse_date(CurrentDate, CurrentYear, CurrentMonth, CurrentDay),
    (ExitDate \= '' ->
        parse_date(ExitDate, ExitYear, ExitMonth, ExitDay),
        TotalYears is ExitYear - HireYear
    ;
        TotalYears is CurrentYear - HireYear
    ).

parse_date(Date, Year, Month, Day) :-
    split_string(Date, "/", "", [MonthStr, DayStr, YearStr]),
    fix_year_str(YearStr, FixedYearStr),  
    atom_number(FixedYearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day).

fix_year_str(YearStr, FixedYearStr) :-
    atom_number(YearStr, YearNum),
    (YearNum > 23 ->
        atom_concat('19', YearStr, FixedYearStr)  % Add 1900 for years over 23
    ;
        atom_concat('20', YearStr, FixedYearStr)  % Add 2000 for years below or equal to 23
    ).

current_date('12/01/23').

