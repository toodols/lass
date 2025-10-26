local style_sheet = Instance.new "StyleSheet"

local style_rule_0 = Instance.new "StyleRule"
style_rule_0.Parent = style_sheet
style_rule_0.Selector = "TextButton, TextLabel"
style_rule_0.Priority = 10
style_rule_0:SetProperties{
	["TextXAlignment"] = Enum.TextXAlignment.Left, 
	["AutomaticSize"] = Enum.AutomaticSize.X, 
	["RichText"] = true, 
	["TextColor3"] = Color3.fromRGB(255, 255, 255), 
	["TextSize"] = 20, 
	["FontFace"] = Font.new "rbxasset://fonts/families/SourceSansPro.json", 
	["BorderSizePixel"] = 0
}

local style_rule_1 = Instance.new "StyleRule"
style_rule_1.Parent = style_sheet
style_rule_1.Selector = "TextButton::UICorner, .solid::UICorner"
style_rule_1:SetProperties{
	["CornerRadius"] = UDim.new(0, 4)
}

local style_rule_2 = Instance.new "StyleRule"
style_rule_2.Parent = style_sheet
style_rule_2.Selector = "Frame"
style_rule_2:SetProperties{
	["BorderSizePixel"] = 0, 
	["BackgroundTransparency"] = 1
}

local style_rule_3 = Instance.new "StyleRule"
style_rule_3.Parent = style_sheet
style_rule_3.Selector = ".solid"
style_rule_3:SetProperties{
	["BackgroundColor3"] = Color3.fromRGB(0, 0, 0), 
	["BackgroundTransparency"] = 0.2
}

style_sheet:SetAttribute("Red", Color3.fromRGB(1, 0, 0))

local style_rule_4 = Instance.new "StyleRule"
style_rule_4.Parent = style_sheet
style_rule_4.Selector = "TextLabel, TextButton"
style_rule_4:SetAttribute("Blue", Color3.fromRGB(0, 0, 1))

local style_rule_5 = Instance.new "StyleRule"
style_rule_5.Parent = style_rule_4
style_rule_5.Selector = " > .title"
style_rule_5:SetProperties{
	["FontFace"] = Font.new("rbxasset://fonts/families/SourceSansPro.json", Enum.FontWeight.Bold, Enum.FontStyle.Normal)
}

local style_rule_6 = Instance.new "StyleRule"
style_rule_6.Parent = style_sheet
style_rule_6.Selector = "ImageLabel"

local style_rule_7 = Instance.new "StyleRule"
style_rule_7.Parent = style_rule_6
style_rule_7.Selector = ":Hover"
style_rule_7.Priority = 1
style_rule_7:SetProperties{
	["ImageTransparency"] = 0.5
}

local style_rule_8 = Instance.new "StyleRule"
style_rule_8.Parent = style_sheet
style_rule_8.Selector = ".a, .b"

local style_rule_9 = Instance.new "StyleRule"
style_rule_9.Parent = style_rule_8
style_rule_9.Selector = ".h, .j"
style_rule_9:SetProperties{
	["E"] = 1
}

return style_sheet