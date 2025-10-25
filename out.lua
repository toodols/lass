local style_sheet = Instance.new "StyleSheet"
local style_rules = {}

local style_rule_0 = Instance.new "StyleRule"
style_rule_0.Parent = style_sheet
table.insert(style_rules, style_rule_0)
style_rule_0.Selector = "TextButton, TextLabel, Frame"
style_rule_0.Priority = 0
style_rule_0:SetProperties{
	["BorderSizePixel"] = 0
}

local style_rule_1 = Instance.new "StyleRule"
style_rule_1.Parent = style_sheet
table.insert(style_rules, style_rule_1)
style_rule_1.Selector = ".solid"
style_rule_1.Priority = 0
style_rule_1:SetProperties{
	["BackgroundColor3"] = Color3.fromRGB(0, 0, 0), 
	["BackgroundTransparency"] = 0.2
}

local style_rule_2 = Instance.new "StyleRule"
style_rule_2.Parent = style_sheet
table.insert(style_rules, style_rule_2)
style_rule_2.Selector = "TextLabel >> .title, TextButton >> .title"
style_rule_2.Priority = 0
style_rule_2:SetProperties{
	["FontFace"] = Font.new("rbxasset://fonts/families/SourceSansPro.json", Enum.FontWeight.Bold, Enum.FontStyle.Normal)
}

local style_rule_3 = Instance.new "StyleRule"
style_rule_3.Parent = style_sheet
table.insert(style_rules, style_rule_3)
style_rule_3.Selector = ".a >> .c >> .e, .a >> .c >> .f, .a >> .d >> .e, .a >> .d >> .f, .b >> .c >> .e, .b >> .c >> .f, .b >> .d >> .e, .b >> .d >> .f"
style_rule_3.Priority = 0
style_rule_3:SetProperties{
	["TextColor3"] = Color3.fromRGB(255,255,255)
}

style_sheet:SetStyleRules(style_rules)